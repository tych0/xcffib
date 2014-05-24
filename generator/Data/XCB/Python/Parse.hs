module Data.XCB.Python.Parse (
  parse,
  mkImport,
  xform,
  renderPy
  ) where

import Control.Monad.State.Strict

import Data.Bits
import qualified Data.Bits.Bitwise as BW
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.XCB.FromXML
import Data.XCB.Types as X
import Data.XCB.Python.PyHelpers

import Language.Python.Common as P

import System.FilePath
import System.FilePath.Glob

import Debug.Trace

data TypeInfo = BaseType String Int
              | CompositeType String (Maybe Int)
              deriving (Eq, Ord, Show)

type TypeInfoMap = M.Map X.Type TypeInfo

parse :: FilePath -> IO [XHeader]
parse fp = do
  files <- globDir1 (compile "*") fp
  fromFiles files

renderPy :: Suite () -> String
renderPy = (intercalate "\n") . map prettyText

xform :: [XHeader] -> [Suite ()]
xform headers =
  let headers' = dependencyOrder headers
  in evalState (mapM processXHeader headers') baseTypeInfo
  where
    processXHeader :: XHeader
                   -> State TypeInfoMap (Suite ())
    processXHeader header = do
      let imports = [mkImport "xcffib", mkImport "struct", mkImport "cStringIO"]
          version = mkVersion header
          key = maybeToList $ mkKey header
      defs <- fmap catMaybes $ mapM processXDecl $ xheader_decls header
      return $ concat [imports, version, key, defs]
    -- Rearrange the headers in dependency order for processing (i.e. put
    -- modules which import others after the modules they import, so typedefs
    -- are propogated appropriately).
    draw = drawForest . map (fmap xheader_header)
    dependencyOrder :: [XHeader] -> [XHeader]
    dependencyOrder headers =
      let forest = unfoldForest unfold $ map xheader_header headers
      in nubBy headerCmp $ concat $ map postOrder forest
      where
        headerM = M.fromList $ map (\h -> (xheader_header h, h)) headers
        unfold s = let h = headerM M.! s in (h, deps h)
        deps :: XHeader -> [String]
        deps = catMaybes . map matchImport . xheader_decls
        matchImport :: XDecl -> Maybe String
        matchImport (XImport n) = Just n
        matchImport _ = Nothing
        headerCmp h1 h2 = (xheader_header h1) == (xheader_header h2)
        postOrder :: Tree a => [a]
        postOrder (Node e cs) = (concat $ map postOrder cs) ++ [e]

-- | Information on basic X types.
baseTypeInfo :: TypeInfoMap
baseTypeInfo = M.fromList $
  [ (UnQualType "CARD8",    BaseType "B" 1)
  , (UnQualType "uint8_t",  BaseType "B" 1)
  , (UnQualType "CARD16",   BaseType "H" 2)
  , (UnQualType "uint16_t", BaseType "H" 2)
  , (UnQualType "CARD32",   BaseType "I" 4)
  , (UnQualType "uint32_t", BaseType "I" 4)
  , (UnQualType "INT8",     BaseType "b" 1)
  , (UnQualType "int8_t",   BaseType "b" 1)
  , (UnQualType "INT16",    BaseType "h" 2)
  , (UnQualType "int16_t",  BaseType "h" 2)
  , (UnQualType "INT32",    BaseType "i" 4)
  , (UnQualType "int32_t",  BaseType "i" 4)
  , (UnQualType "BYTE",     BaseType "B" 1)
  , (UnQualType "BOOL",     BaseType "B" 1)
  , (UnQualType "char",     BaseType "b" 1)
  , (UnQualType "void",     BaseType "B" 1)
  , (UnQualType "float",    BaseType "f" 4)
  , (UnQualType "double",   BaseType "d" 8)
  ]

getName :: X.Type -> String
getName (UnQualType n) = n
getName (QualType n1 n2) = n1 ++ "." ++ n2

xBinopToPyOp :: X.Binop -> P.Op ()
xBinopToPyOp X.Add = P.Plus ()
xBinopToPyOp X.Sub = P.Minus ()
xBinopToPyOp X.Mult = P.Multiply ()
xBinopToPyOp X.Div = P.Divide ()
xBinopToPyOp X.And = P.And ()
xBinopToPyOp X.RShift = P.ShiftRight ()

xUnopToPyOp :: X.Unop -> P.Op ()
xUnopToPyOp X.Complement = P.Invert ()

xExpressionToPyExpr :: XExpression -> Expr ()
xExpressionToPyExpr (Value i) = mkInt i
xExpressionToPyExpr (Bit i) = mkInt $ shiftL 1 i
xExpressionToPyExpr (FieldRef n) = mkAttr n
xExpressionToPyExpr (EnumRef _ n) = mkVar n
xExpressionToPyExpr (PopCount e) = mkCall "popcount" [xExpressionToPyExpr e]
xExpressionToPyExpr (PopCount _) = error "Bad X spec?"
-- TODO: What do we do for SumOf, besides cause a NameError?
xExpressionToPyExpr (SumOf _) = mkVar "xcffib_incomplete"
xExpressionToPyExpr (Op o e1 e2) =
  let o' = xBinopToPyOp o
      e1' = xExpressionToPyExpr e1
      e2' = xExpressionToPyExpr e2
  in BinaryOp o' e1' e2' ()
xExpressionToPyExpr (Unop o e) =
  let o' = xUnopToPyOp o
      e' = xExpressionToPyExpr e
  in UnaryOp o' e' ()

xEnumElemsToPyEnum :: [XEnumElem] -> [(String, Expr ())]
xEnumElemsToPyEnum membs = reverse $ conv membs [] [1..]
  where
    conv :: [XEnumElem] -> [(String, Expr ())] -> [Int] -> [(String, Expr ())]
    conv ((EnumElem name expr) : els) acc is =
      let expr' = fromMaybe (mkInt (head is)) $ fmap xExpressionToPyExpr expr
          is' = tail is
          acc' = (name, expr') : acc
      in conv els acc' is'
    conv [] acc _ = acc

structElemToPyUnpack :: TypeInfoMap
                     -> GenStructElem Type
                     -> Either (Maybe String, String, Maybe Int)
                               (Statement (), Expr ())
structElemToPyUnpack _ (Pad i) = Left (Nothing, (show i) ++ "x", Just i)

-- The enum field is mostly for user information, so we ignore it.
structElemToPyUnpack m (X.List n typ (Just expr) _) =
  let len = xExpressionToPyExpr expr
      (c, i) = case m M.! typ of
                 BaseType c i -> (mkStr c, Just i)
                 CompositeType c i -> (mkName c, i)
      size = map mkInt $ maybeToList i
      list = mkCall "xcb.List" ([ (mkName "parent")
                                , (mkName "offset")
                                , len
                                , c
                                ] ++ size)
      assign = mkAssign (mkAttr n) list
      totalBytes = mkAttr (n ++ ".bufsize")
  in Right (assign, totalBytes)

structElemToPyUnpack _ (X.List n typ Nothing _) =
  error ("Invalid XCB XML; list " ++ n ++ " requires a length")

-- The mask and enum fields are for user information, we can ignore them here.
structElemToPyUnpack m (SField n typ _ _) =
  case m M.! typ of
    BaseType c i -> Left (Just n, c, Just i)
    CompositeType c i ->
      let size = map mkInt $ maybeToList i
          assign = mkAssign (mkAttr n) (mkCall c ([ mkName "parent"
                                                  , mkName "offset"
                                                  ] ++ size))
      in Right (assign, mkAttr (n ++ ".buflen"))
structElemToPyUnpack _ (ExprField _ _ _) = error "Only valid for requests"
structElemToPyUnpack _ (ValueParam _ _ _ _) = error "Only valid for requests"

processXDecl :: XDecl
             -> State TypeInfoMap (Maybe (Statement ()))
processXDecl (XTypeDef name typ) =
  do modify $ \m -> M.insert (UnQualType name) (m M.! typ) m
     return Nothing
processXDecl (XidType name) =
  -- http://www.markwitmer.com/guile-xcb/doc/guile-xcb/XIDs.html
  do modify $ M.insert (UnQualType name) (BaseType "I" 4)
     return Nothing
processXDecl (XImport n) =
  return $ return $ mkImport n
processXDecl (XEnum name membs) =
  return $ return $ mkEnum name $ xEnumElemsToPyEnum membs
processXDecl (XStruct n membs) = do
  m <- get
  let (toUnpack, lists) = partitionEithers $ map (structElemToPyUnpack m) membs
      -- XXX: Here we assume that all the lists come after all the unpacked
      -- members. While (I think) this is true today, it may not always be
      -- true and we should probably fix this.
      (names, packs, lengths) = unzip3 toUnpack
      assign = mkUnpackFrom (catMaybes names) packs
      unpackLength = sum $ catMaybes lengths
      incr = mkIncr "offset" $ mkInt unpackLength
      lists' = concat $ map (\(l, sz) -> [l, mkIncr "offset" sz]) lists
      structLen = if length lists > 0 then Nothing else Just unpackLength
  modify $ M.insert (UnQualType n) (CompositeType n structLen)
  return $ return $ mkClass n "xcffib.Protobj" $ [assign, incr] ++ lists'
processXDecl (XEvent name number membs hasSequence) = return Nothing
processXDecl (XRequest name number membs reply) = return Nothing
processXDecl (XUnion name membs) = do
  m <- get
  let (fields, lists) = partitionEithers $ map (structElemToPyUnpack m) membs
      (toUnpack, sizes) = unzip $ map mkUnionUnpack fields
      (lists', lengths) = unzip lists
      err = error ("bad XCB: union " ++
                   name ++ " has fields of different length")
      lengths' = catMaybes $ nub sizes
      unionLen = if length lists' > 0 then Nothing else listToMaybe lengths'
  -- There should be at most one size of object in the struct.
  unless ((length $ lengths') <= 1) err
  -- List in list, so we don't know a length here. -1 is the sentinel value
  -- xpyb uses for this.
  modify $ M.insert (UnQualType name) (CompositeType name unionLen)
  return $ Just $ mkClass name "xcffib.Union" $ (fst $ unzip lists) ++ toUnpack
  where
    mkUnionUnpack :: (Maybe String, String, Maybe Int)
                  -> (Statement (), Maybe Int)
    mkUnionUnpack (name, typ, size) =
      (mkUnpackFrom (maybeToList name) [typ], size)

processXDecl (XidUnion name _) =
  -- These are always unions of only XIDs.
  do modify $ M.insert (UnQualType name) (BaseType "I" 4)
     return Nothing
processXDecl (XError name number membs) = return Nothing

mkVersion :: XHeader -> Suite ()
mkVersion header =
  let major = ver "MAJOR_VERSION" (xheader_major_version header)
      minor = ver "MINOR_VERSION" (xheader_minor_version header)
  in major ++ minor
  where
    ver :: String -> Maybe Int -> Suite ()
    ver target i = maybeToList $ fmap (\x -> mkAssign target (mkInt x)) i

mkKey :: XHeader -> Maybe (Statement ())
mkKey header = do
  name <- xheader_xname header
  let call = mkCall "xcffib.ExtensionKey" [mkStr name]
  return $ mkAssign "key" call

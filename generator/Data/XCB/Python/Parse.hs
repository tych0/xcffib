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

parse :: FilePath -> IO [XHeader]
parse fp = do
  files <- globDir1 (compile "*") fp
  fromFiles files

renderPy :: Suite () -> String
renderPy = (intercalate "\n") . map prettyText

xform :: [XHeader] -> [Suite ()]
xform headers =
  let headers' = dependencyOrder headers
  in evalState (mapM processXHeader headers') M.empty
  where
    processXHeader :: XHeader
                   -> State (M.Map String (String, Maybe Int)) (Suite ())
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
baseTypeInfo :: M.Map X.Type (String, Int)
baseTypeInfo = M.fromList $
  [ (UnQualType "CARD8",    ("B", 1))
  , (UnQualType "uint8_t",  ("B", 1))
  , (UnQualType "CARD16",   ("H", 2))
  , (UnQualType "uint16_t", ("H", 2))
  , (UnQualType "CARD32",   ("I", 4))
  , (UnQualType "uint32_t", ("I", 4))
  , (UnQualType "INT8",     ("b", 1))
  , (UnQualType "int8_t",   ("b", 1))
  , (UnQualType "INT16",    ("h", 2))
  , (UnQualType "int16_t",  ("h", 2))
  , (UnQualType "INT32",    ("i", 4))
  , (UnQualType "int32_t",  ("i", 4))
  , (UnQualType "BYTE",     ("B", 1))
  , (UnQualType "BOOL",     ("B", 1))
  , (UnQualType "char",     ("b", 1))
  , (UnQualType "void",     ("B", 1))
  , (UnQualType "float",    ("f", 4))
  , (UnQualType "double",   ("d", 8))
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
xExpressionToPyExpr (PopCount (Value i)) =
  mkInt $ length $ filter id $ BW.toListLE i
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

structElemToPyUnpack :: M.Map String (String, Maybe Int)
                     -> GenStructElem Type
                     -> Either (Maybe String, String, Maybe Int)
                               (Statement (), Expr ())
structElemToPyUnpack _ (Pad i) = Left (Nothing, (show i) ++ "x", Just i)

-- The enum field is mostly for user information, so we ignore it.
structElemToPyUnpack m (X.List n typ (Just expr) _) =
  let len = xExpressionToPyExpr expr
      name = getName typ
      (baseC, baseI) = baseTypeInfo M.! typ
      (c, i) = M.findWithDefault (baseC, Just baseI) name m
      size = map mkInt $ maybeToList i
      list = mkCall "xcb.List" ([ (mkName "parent")
                                , (mkName "offset")
                                , len
                                , mkStr c
                                ] ++ size)
      assign = mkAssign (mkAttr n) list
      totalBytes = mkAttr (name ++ ".bufsize")
  in Right (assign, totalBytes)

structElemToPyUnpack _ (X.List n typ Nothing _) =
  error ("Invalid XCB XML; list " ++ n ++ " requires a length")

-- The mask and enum fields are for user information, we can ignore them here.
structElemToPyUnpack m (SField n typ _ _) =
  let (baseC, baseI) = baseTypeInfo M.! typ
      (c, i) = M.findWithDefault (baseC, Just baseI) (getName typ) m
  in Left (Just n, c, i)
structElemToPyUnpack _ (ExprField _ _ _) = error "Only valid for requests"
structElemToPyUnpack _ (ValueParam _ _ _ _) = error "Only valid for requests"

processXDecl :: XDecl
             -> State (M.Map String (String, Maybe Int)) (Maybe (Statement ()))
processXDecl (XTypeDef name typ) =
  do m <- get
     let (baseC, baseI) = baseTypeInfo M.! typ
     put $ M.insert name (baseC, Just baseI) m
     return Nothing
processXDecl (XidType name) =
  do m <- get
     -- http://www.markwitmer.com/guile-xcb/doc/guile-xcb/XIDs.html
     put $ M.insert name ("I", Just 4) m
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
  put $ M.insert n (n, structLen) m
  return $ return $ mkClass n "xcffib.Protobj" $ [assign, incr] ++ lists'
processXDecl (XEvent name number membs hasSequence) = return Nothing
processXDecl (XRequest name number membs reply) = return Nothing
processXDecl (XUnion name membs) = do
  m <- get
  -- XXX: As far as I can tell all the (one) unions in XCB are lists. It's not
  -- clear to me what the semantics of multiple members would be, so we fail
  -- hard here.
  let ([], lists) = partitionEithers $ map (structElemToPyUnpack m) membs
      (lists', lengths) = unzip lists
  -- List in list, so we don't know a length here. -1 is the sentinel value
  -- xpyb uses for this.
  put $ M.insert name (name, Nothing) m
  return $ Just $ mkClass name "xcffib.Union" $ fst $ unzip lists
processXDecl (XidUnion name _) =
  -- These are always only unions of XIDs.
  do m <- get
     put $ M.insert name ("I", Just 4) m
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

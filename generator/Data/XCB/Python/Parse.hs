module Data.XCB.Python.Parse (
  parse,
  emit,
  mkImport,
  xform,
  renderPy
  ) where

import Data.Bits
import qualified Data.Bits.Bitwise as BW
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.XCB.FromXML
import Data.XCB.Types as X
import Data.XCB.Python.PyHelpers

import Language.Python.Common as P

import System.FilePath
import System.FilePath.Glob

parse :: FilePath -> IO [XHeader]
parse fp = do
  files <- globDir1 (compile "*") fp
  fromFiles files

emit :: XHeader -> FilePath -> IO ()
emit header dir = do
  let fname = dir </> xheader_header header ++ ".py"
  writeFile fname $ renderPy $ xform header

renderPy :: Suite () -> String
renderPy = (intercalate "\n") . map prettyText

xform :: XHeader -> Suite ()
xform header =
  let imports = [mkImport "xcffib", mkImport "struct", mkImport "cStringIO"]
      decls = xheader_decls header
      typeInfo = mkTypeInfo $ collectTypes decls
      decls = catMaybes $ map (processXDecl typeInfo) $ xheader_decls header
      version = mkVersion header
      key = maybeToList $ mkKey header
  in concat [imports, decls, version, key]

-- | Get the type info (python's struct.pack string and size).
mkTypeInfo :: Map X.Type X.Type -> X.Type -> (String, Int)
mkTypeInfo typedefs = fromMaybe (baseType t) $ fmap lookup $ M.lookup t typedefs
  where
    baseType :: X.Type -> (String, Int)
    baseType (UnQualType "CARD8")    = ("B", 1)
    baseType (UnQualType "uint8_t")  = ("B", 1)
    baseType (UnQualType "CARD16")   = ("H", 2)
    baseType (UnQualType "uint16_t") = ("H", 2)
    baseType (UnQualType "CARD32")   = ("I", 4)
    baseType (UnQualType "uint32_t") = ("I", 4)
    baseType (UnQualType "INT8")     = ("b", 1)
    baseType (UnQualType "int8_t")   = ("b", 1)
    baseType (UnQualType "INT16")    = ("h", 2)
    baseType (UnQualType "int16_t")  = ("h", 2)
    baseType (UnQualType "INT32")    = ("i", 4)
    baseType (UnQualType "int32_t")  = ("i", 4)
    baseType (UnQualType "BYTE")     = ("B", 1)
    baseType (UnQualType "BOOL")     = ("B", 1)
    baseType (UnQualType "char")     = ("b", 1)
    baseType (UnQualType "void")     = ("B", 1)
    baseType (UnQualType "float")    = ("f", 4)
    baseType (UnQualType "double")   = ("d", 8)
    baseType t = error ("unknown type " ++ show t)

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

structElemToPyUnpack :: X.Type -> (String, Int)
                     -> GenStructElem Type
                     -> Either (Maybe String, String, Int) (Suite ())
structElemToPyUnpack _ (Pad i) = Left (Nothing, (show i) ++ "x", i)

-- The enum field is mostly for user information, so we ignore it.
structElemToPyUnpack typeInfo (X.List n typ (Just exp) _) =
  let len = xExpressionToPyExpr exp
      (c, i) = typeInfo typ
      list = mkCall "xcb.List" [ (mkName "parent")
                               , (mkName "offset")
                               , len
                               , mkStr c
                               , mkInt i
                               ]
      assign = mkAssign (mkAttr n) list
      totalBytes = BinaryOp (Multiply ()) (mkCall "len" [mkAttr n]) (mkInt i) ()
      incr = mkIncr "offset" totalBytes
  in Right [assign, incr]

structElemToPyUnpack _ (X.List n typ Nothing _) =
  error ("Invalid XCB XML; list " ++ n ++ " requires a length")

-- The mask and enum fields are for user information, we can ignore them here.
structElemToPyUnpack typeInfo (SField n typ _ _) =
  let (c, i) = typeInfo typ in Left (Just n, c, i)
structElemToPyUnpack _ (ExprField _ _ _) = error "Only valid for requests"
structElemToPyUnpack _ (ValueParam _ _ _ _) = error "Only valid for requests"

processXDecl :: (X.Type -> (String, Int)) -> XDecl -> Maybe (Statement ())
processXDecl _ (XTypeDef _ _) = Nothing
processXDecl _ (XidType _) = Nothing
processXDecl _ (XImport n) = return $ mkImport n
processXDecl _ (XEnum name membs) = return $ mkEnum name $ xEnumElemsToPyEnum membs
processXDecl typeInfo (XStruct n membs) = do
  let (toUnpack, lists) = partitionEithers $ map (structElemToPyUnpack typeInfo) membs
      -- XXX: Here we assume that all the lists come after all the unpacked
      -- members. While (I think) this is true today, it may not always be
      -- true and we should probably fix this.
      (names, packs, lengths) = unzip3 toUnpack
      assign = mkUnpackFrom (catMaybes names) packs
      incr = mkIncr "offset" $ mkInt $ sum lengths
  return $ mkClass cname "xcffib.Protobj" $ [assign, incr] ++ concat lists
processXDecl typeInfo (XEvent n membs hasSequence) = Nothing
processXDecl typeInfo (XRequest n membs reply) = Nothing
processXDecl typeInfo (XUnion n membs) = Nothing
processXDecl typeInfo (XError n membs) = Nothing

collectTypes :: [XDecl] -> Map X.Type X.Type
collectTypes = foldr collectType M.empty
  where
    collectType :: XDecl -> Map X.Type X.Type -> Map X.Type X.Type
    collectType (XTypeDef name typ) = insert (UnQualType name) typ
    -- http://www.markwitmer.com/guile-xcb/doc/guile-xcb/XIDs.html
    collectType (XidType name) = insert (UnQualType name) (UnQualType "CARD32")
    collectType _ = id

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

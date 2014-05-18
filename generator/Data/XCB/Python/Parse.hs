module Data.XCB.Python.Parse (
  parse,
  emit,
  mkImport,
  xform,
  renderPy
  ) where

import Data.Bits
import qualified Data.Bits.Bitwise as BW
import Data.List
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
      decls = catMaybes $ map processXDecl $ xheader_decls header
      version = mkVersion header
      key = maybeToList $ mkKey header
  in concat [imports, decls, version, key]

-- | Get the type info (python's struct.pack string and size).
typeInfo :: X.Type -> (String, Int)
typeInfo (UnQualType "CARD8") = ("B", 1)
typeInfo t = error ("unknown type: " ++ show t)

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

structElemToPyUnpack :: GenStructElem Type
                     -> Either (Maybe String, String, Int) (Suite ())
structElemToPyUnpack (Pad i) = Left (Nothing, (show i) ++ "x", i)

-- The enum field is mostly for user information, so we ignore it.
structElemToPyUnpack (X.List n typ (Just exp) _) =
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

structElemToPyUnpack (X.List n typ Nothing _) =
  error ("Invalid XCB XML; list " ++ n ++ " requires a length")

-- The mask and enum fields are for user information, we can ignore them here.
structElemToPyUnpack (SField n typ _ _) =
  let (c, i) = typeInfo typ in Left (Just n, c, i)
structElemToPyUnpack (ExprField _ _ _) = error "Only valid for requests"
structElemToPyUnpack (ValueParam _ _ _ _) = error "Only valid for requests"

xStructToPyClass :: String -> [GenStructElem Type] -> Statement ()
xStructToPyClass cname membs = undefined

processXDecl :: XDecl -> Maybe (Statement ())
processXDecl (XImport n) = Just $ mkImport n
processXDecl (XEnum name membs) = Just $ mkEnum name $ xEnumElemsToPyEnum membs
processXDecl (XStruct n elems) = Nothing
processXDecl _ = Nothing

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

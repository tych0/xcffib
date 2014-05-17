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
      key = [mkKey header]
  in concat [imports, decls, version, key]

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
xExpressionToPyExpr (FieldRef n) = mkVar n
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

processXDecl :: XDecl -> Maybe (Statement ())
processXDecl (XImport n) = Just $ mkImport n
processXDecl (XEnum name membs) = Just $ mkEnum name $ xEnumElemsToPyEnum membs
processXDecl _ = Nothing

mkVersion :: XHeader -> Suite ()
mkVersion header =
  let major = ver "MAJOR_VERSION" (xheader_major_version header)
      minor = ver "MINOR_VERSION" (xheader_minor_version header)
  in major ++ minor
  where
    ver :: String -> Maybe Int -> Suite ()
    ver target i = maybeToList $ fmap (\x -> mkAssign target (mkInt x)) i

mkKey :: XHeader -> Statement ()
mkKey header =
  let Just name = xheader_xname header
      args = [mkArg $ Strings [name] ()]
      call = mkCall "xcffib.ExtensionKey" args
  in mkAssign "key" call

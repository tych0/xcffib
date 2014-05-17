module Data.XCB.Python.Parse (
  parse,
  emit,
  mkImport,
  xform,
  renderPy
  ) where

import Data.List
import Data.Maybe
import Data.XCB.FromXML
import Data.XCB.Types
import Data.XCB.Python.PyHelpers

import Language.Python.Common

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
      xcbImports = getXImports header
      version = mkVersion header
      key = [mkKey header]
  in concat [imports, xcbImports, version]

getXImports :: XHeader -> Suite ()
getXImports header = catMaybes $ map getXImport $ xheader_decls header
  where
    getXImport :: GenXDecl a -> Maybe (Statement ())
    getXImport (XImport n) = Just $ mkImport n
    getXImport _ = Nothing

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

mkErrors :: XHeader -> Suite ()
mkErrors = undefined

mkRequests :: XHeader -> Suite ()
mkRequests = undefined

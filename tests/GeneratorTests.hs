module Main (main) where

import Language.Python.Common

import Data.XCB.Python.Parse
import Data.XCB.FromXML
import Data.XCB.Types

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )

import System.FilePath

tests :: [String]
tests = [ "event"
        , "error"
        , "request"
        , "union"
        , "struct"
        , "enum"
        , "request_reply"
        , "no_sequence"
        ]

mkFname :: String -> FilePath
mkFname = (</>) $ "tests" </> "generator"

mkTest :: String -> IO Test
mkTest name = do
  header <- fromFiles [mkFname $ name <.> ".xml"]
  rawExpected <- readFile . mkFname $ name <.> ".py"
  let [(fname, outPy)] = xform header
      rawOut = renderPy outPy
  return $ testCase name $ do assertEqual "names equal" name fname
                              -- TODO: we should really parse and compare ASTs
                              assertEqual "rendering equal" rawExpected rawOut

main :: IO ()
main = mapM mkTest tests >>= defaultMain

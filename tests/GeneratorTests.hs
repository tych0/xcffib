module Main (main) where

import Language.Python.Common

import Data.XCB.Python.Parse
import Data.XCB.FromXML
import Data.XCB.Types

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )

import System.FilePath

pyTests :: [String]
pyTests = [ "event"
          , "error"
          , "request"
          , "union"
          , "struct"
          , "enum"
          , "request_reply"
          , "no_sequence"
          , "type_pad"
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


calcsizeTests :: [Test]
calcsizeTests =
  let tests = [ ("x2xBx", 5)
              , ("24xHhII", 24 + 2 * 2 + 2 * 4)
              ]
  in map mkTest tests
  where
    mkTest (str, expected) =
      let result = calcsize str
      in testCase "calcsize" (assertEqual str expected result)

main :: IO ()
main = do
  genTests <- mapM mkTest pyTests
  defaultMain $ calcsizeTests ++ genTests

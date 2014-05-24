module Main (main) where

import Language.Python.Common

import Data.XCB.Python.PyHelpers

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )

testMkName :: Test
testMkName =
  let result = mkName "self.foo.bar"
      expected = BinaryOp (Dot ())
                          (Var (Ident "self" ()) ())
                          (BinaryOp (Dot ())
                                    (Var (Ident "foo" ()) ())
                                    (Var (Ident "bar" ()) ())
                                    ())
                          ()
  in testCase "testMkName" (assertEqual "testMkName" expected result)

main :: IO ()
main = do
  defaultMain [testMkName]

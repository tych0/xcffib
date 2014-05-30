module Main (main) where

import Language.Python.Common

import Data.XCB.Python.PyHelpers

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )

mkTest :: (Show a, Eq a) => String -> a -> a -> Test
mkTest name t1 t2 = testCase name (assertEqual name t1 t2)

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
  in mkTest "testMkName" expected result

testReserves :: Test
testReserves =
  let result = mkName "None"
      expected = (Var (Ident "_None" ()) ())
  in mkTest "testReserves" expected result

main :: IO ()
main = do
  defaultMain [testMkName, testReserves]

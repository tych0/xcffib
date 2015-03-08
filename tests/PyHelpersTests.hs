{-
 - Copyright 2014 Tycho Andersen
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -   http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}
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
      expected = (Dot (Dot (Var (Ident "self" ()) ())
                           (Ident "foo" ()) ())
                      (Ident "bar" ()) ())
  in mkTest "testMkName" expected result

testReserves :: Test
testReserves =
  let result = mkName "None"
      expected = (Var (Ident "_None" ()) ())
  in mkTest "testReserves" expected result

main :: IO ()
main = do
  defaultMain [testMkName, testReserves]

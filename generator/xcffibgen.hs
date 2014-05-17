module Main where

import Data.XCB.Python.Parse

import Language.Python.Common

main :: IO ()
main = do
  let imp = mkImport "StringIO"
  putStrLn $ prettyText warning
  putStrLn $ prettyText imp

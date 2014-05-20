module Main where

import Data.XCB.Python.Parse
import Data.XCB.Types

main :: IO [()]
main = do
  headers <- parse "/usr/share/xcb"
  let headersAndNames = zip (map xheader_name headers) (xform headers)
  sequence $ map printHeader headersAndNames
  where
    printHeader (name, py) = do
      putStrLn (show name)
      putStrLn $ renderPy py

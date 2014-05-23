module Main where

import Data.XCB.Python.Parse
import Data.XCB.Types

main :: IO [()]
main = do
  headers <- parse "/usr/share/xcb"
  sequence $ map (putStrLn . renderPy) $ xform headers

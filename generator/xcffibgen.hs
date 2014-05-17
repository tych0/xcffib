module Main where

import Data.XCB.Python.Parse
import Data.XCB.Types

main :: IO [()]
main = do
  headers <- parse "/usr/share/xcb"
  sequence $ map renderHeader headers
  where
    renderHeader :: XHeader -> IO ()
    renderHeader header = do
      putStrLn $ "\nextension:" ++ (xheader_header header)
      putStrLn $ renderPy $ xform header

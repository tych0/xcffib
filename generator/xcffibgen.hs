module Main where

import Data.XCB.Types
import Data.XCB.Python.Parse

import Options.Applicative

import System.Directory
import System.FilePath

data Xcffibgen = Xcffibgen { input :: String
                           , output :: String
                           }

options :: Parser Xcffibgen
options = Xcffibgen
    <$> strOption
        ( long "input"
       <> metavar "DIR"
       <> help "Input directory containing xcb xml files.")
    <*> strOption
        ( long "output"
       <> metavar "DIR"
       <> help "Output directory for generated python.")

-- Headers we can't emit right now. Obviously we want to get rid of this :-)
badHeaders :: [String]
badHeaders = [ "xkb"
             , "xprint"
             ]

run :: Xcffibgen -> IO ()
run (Xcffibgen inp out) = do
  headers <- parse inp
  let headers' = filter (flip notElem badHeaders . xheader_header) headers
  createDirectoryIfMissing True out
  sequence_ $ map processFile $ xform headers'
  where
    processFile (fname, suite) = do
      putStrLn fname
      let fname' = out </> fname ++ ".py"
          contents = renderPy suite
      writeFile fname' contents

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Generate XCB bindings for python."
     <> header "xcffib - the cffi-based XCB generator")

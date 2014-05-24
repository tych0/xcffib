module Main where

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

run :: Xcffibgen -> IO ()
run (Xcffibgen inp out) = do
  headers <- parse inp
  createDirectoryIfMissing True out
  sequence_ $ map processFile $ xform headers
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

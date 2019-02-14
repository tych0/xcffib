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
  headers <- parseXHeaders inp
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

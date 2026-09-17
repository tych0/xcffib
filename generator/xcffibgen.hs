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

import Data.XCB.Python.Parse

import Options.Applicative

import System.Directory
import System.FilePath

import Control.Monad (mfilter)
import Data.Char (isAlphaNum, isHexDigit)
import Data.List (isPrefixOf, sort, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment (lookupEnv)
import System.Process (readProcess)

keysymSections :: [String]
keysymSections =
  [ "MISCELLANY", "XKB_KEYS", "3270", "LATIN1", "LATIN2", "LATIN3"
  , "LATIN4", "LATIN8", "LATIN9", "KATAKANA", "ARABIC", "CYRILLIC"
  , "GREEK", "TECHNICAL", "SPECIAL", "PUBLISHING", "APL", "HEBREW"
  , "THAI", "KOREAN", "ARMENIAN", "GEORGIAN", "CAUCASUS", "VIETNAMESE"
  , "CURRENCY", "MATHEMATICAL", "BRAILLE", "SINHALA"
  ]

generateKeysyms :: FilePath -> IO ()
generateKeysyms out = do
  cpp <- fromMaybe "cpp" <$> lookupEnv "CPP"
  let generate outputName headerFile extraArgs shouldKeep = do
        defines <- lines <$> readProcess cpp
          (["-dM"] ++ extraArgs ++ ["-include", headerFile, "-"]) ""
        writeFile (out </> outputName) . unlines . sort $
          mapMaybe (fmap formatDefine . mfilter (shouldKeep . fst) . parseDefine) defines
  generate "keysymdef.py" "X11/keysymdef.h" (map ("-DXK_" ++) keysymSections) $
    maybe False (`notElem` keysymSections) . stripPrefix "XK_"
  generate "xf86keysym.py" "X11/XF86keysym.h" [] $ isPrefixOf "XF86XK_"
  where
    parseDefine line = case words line of
      ("#define" : name : vals)
        | validName name -> Just (name, unwords vals)
      _ -> Nothing

    validName name = not (null name) && all (\c -> isAlphaNum c || c == '_') name

    formatDefine (name, val) = name ++ " = " ++ renderValue val

    renderValue val = case val of
      '0' : 'x' : digits
        | not (null digits) && all isHexDigit digits -> val
      _ -> case stripPrefix "_EVDEVK(0x" val >>= (fmap reverse . stripPrefix ")" . reverse) of
        Just digits
          | not (null digits) && all isHexDigit digits ->
              "0x10081000 + 0x" ++ digits
        _ -> error $ "invalid keysym value: " ++ val

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
  headers <- parseXHeaders inp
  createDirectoryIfMissing True out
  sequence_ $ map processFile $ xform headers
  generateKeysyms out
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

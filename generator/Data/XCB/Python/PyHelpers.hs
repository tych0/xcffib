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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.XCB.Python.PyHelpers (
  mkImport,
  mkRelImport,
  mkInt,
  mkAssign,
  mkCall,
  noArgs,
  mkArg,
  mkEnum,
  mkName,
  mkDot,
  mkAttr,
  mkIncr,
  mkClass,
  mkEmptyClass,
  mkXClass,
  mkStr,
  mkUnpackFrom,
  mkDict,
  mkDictUpdate,
  mkMethod,
  mkReturn,
  pyTruth,
  mkParams,
  ident,
  pyNone,
  mkIf,
  repeatStr
  ) where

import Data.List.Split
import Data.Maybe

import Language.Python.Common

_reserved :: [String]
_reserved = [ "None"
            , "def"
            , "class"
            , "and"
            , "or"
            ]

class PseudoExpr a where
  getExpr :: a -> Expr ()

instance PseudoExpr String where
  getExpr s = mkName s
instance PseudoExpr (Expr ()) where
  getExpr = id

-- | Create and sanatize a python identifier.
ident :: String -> Ident ()
ident s | s `elem` _reserved = Ident ("_" ++ s) ()
ident s | isInt s = Ident ("_" ++ s) ()
  where
    isInt str = isJust $ ((maybeRead str) :: Maybe Int)
    maybeRead = fmap fst . listToMaybe . reads
ident s = Ident s ()

-- Make a DottedName out of a string like "foo.bar" for use in imports.
mkDottedName :: String -> DottedName ()
mkDottedName = map ident . splitOn "."

mkVar :: String -> Expr ()
mkVar name = Var (ident name) ()

-- | Make an Expr out of a string like "foo.bar" describing the name.
mkName :: String -> Expr ()
mkName s =
  let strings = splitOn "." s
  in foldl mkDot (mkVar $ head strings) (tail strings)

mkDot :: PseudoExpr a => a -> String -> Expr ()
mkDot e1 attr = Dot (getExpr e1) (ident attr) ()

-- | Make an attribute access, i.e. self.<string>.
mkAttr :: String -> Expr ()
mkAttr s = mkName ("self." ++ s)

mkImport :: String -> Statement ()
mkImport name = Import [ImportItem (mkDottedName name) Nothing ()] ()

mkRelImport :: String -> Statement ()
mkRelImport name = FromImport (ImportRelative 1 Nothing ()) (FromItems [FromItem (ident name) Nothing ()] ()) ()

mkInt :: Int -> Expr ()
mkInt i = Int (toInteger i) (show i) ()

mkAssign :: PseudoExpr a => a -> Expr () -> Statement ()
mkAssign name expr = Assign [getExpr name] expr ()

mkIncr :: String -> Expr () -> Statement ()
mkIncr name expr = AugmentedAssign (mkName name) (PlusAssign ()) expr ()

class PseudoArgument a where
  getArgument :: a -> Argument ()

instance PseudoArgument (Expr ()) where
  getArgument p = ArgExpr p ()
instance PseudoArgument (Argument ()) where
  getArgument = id

mkCall :: (PseudoExpr a, PseudoArgument b) => a -> [b] -> Expr ()
mkCall name args = Call (getExpr name) (map getArgument args) ()

noArgs :: [Argument ()]
noArgs = []

mkEnum :: String -> [(String, Expr ())] -> Statement ()
mkEnum cname values =
  let body = map (uncurry mkAssign) values
  in Class (Ident cname ()) [] body ()

mkParams :: [String] -> [Parameter ()]
mkParams = map (\x -> Param (ident x) Nothing Nothing ())

mkArg :: String -> Argument ()
mkArg n = ArgExpr (mkName n) ()

mkXClass :: String -> String -> Suite () -> Suite () -> Statement ()
mkXClass clazz superclazz [] [] = mkEmptyClass clazz superclazz
mkXClass clazz superclazz constructor methods =
  let args = [ "self", "unpacker" ]
      super = mkCall (superclazz ++ ".__init__") $ map mkName args
      body = eventToUnpacker : (StmtExpr super ()) : constructor
      initParams = mkParams args
      initMethod = Fun (ident "__init__") initParams Nothing body ()
  in mkClass clazz superclazz $ initMethod : methods

    where

      -- In some cases (e.g. when creating ClientMessageEvents), our events are
      -- passed directly to __init__. Since we don't keep track of the
      -- underlying buffers after the event is created, we have to re-pack
      -- things so they can be unpacked again.
      eventToUnpacker :: Statement ()
      eventToUnpacker = let newUnpacker = mkAssign "unpacker" (mkCall "xcffib.MemoryUnpacker"
                                                              [mkCall "unpacker.pack" noArgs])
                            cond = mkCall "isinstance" [mkName "unpacker", mkName "xcffib.Protobj"]
                        in mkIf cond [newUnpacker]


mkEmptyClass :: String -> String -> Statement ()
mkEmptyClass clazz superclazz = mkClass clazz superclazz [Pass ()]

mkClass :: String -> String -> Suite () -> Statement ()
mkClass clazz superclazz body = Class (ident clazz) [mkArg superclazz] body ()

mkStr :: String -> Expr ()
mkStr s = Strings ["\"", s, "\""] ()

mkTuple :: [Expr ()] -> Expr ()
mkTuple = flip Tuple ()

mkUnpackFrom :: PseudoExpr a => a -> [String] -> String -> Suite ()
mkUnpackFrom unpacker names packs =
  let lhs = mkTuple $ map mkAttr names
      -- Don't spam with this default arg unless it is really necessary.
      unpackF = mkDot unpacker "unpack"
      rhs = mkCall unpackF [mkStr packs]
      stmt = if length names > 0 then mkAssign lhs rhs else StmtExpr rhs ()
  in if length packs > 0 then [stmt] else []

mkDict :: String -> Statement ()
mkDict name = mkAssign name (Dictionary [] ())

mkDictUpdate :: String -> Int -> String -> Statement ()
mkDictUpdate dict key value =
  mkAssign (Subscript (mkName dict) (mkInt key) ()) (mkName value)

mkMethod :: String -> [Parameter ()] -> Suite () -> Statement ()
mkMethod name args body = Fun (ident name) args Nothing body ()

mkReturn :: Expr () -> Statement ()
mkReturn = flip Return () . Just

pyTruth :: Bool -> Expr ()
pyTruth = flip Bool ()

pyNone :: Expr ()
pyNone = None ()

mkIf :: Expr () -> Suite () -> Statement ()
mkIf e s = Conditional [(e, s)] [] ()

repeatStr :: String -> Expr () -> Expr ()
repeatStr s i = BinaryOp (Multiply ()) (mkStr s) i ()

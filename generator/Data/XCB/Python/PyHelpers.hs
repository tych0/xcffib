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
  mkRelImport,
  mkAssign,
  mkCall,
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
  mkIf,
  notImplemented
  ) where

import Data.List.Split

import Data.XCB.Python.AST (Expr(..), Op(..), Statement(..), Suite, Ident, PseudoExpr, getExpr)

-- | Make an Expr out of a string like "foo.bar" describing the name.
mkName :: String -> Expr
mkName s =
  let strings = splitOn "." s
  in foldl mkDot (Var $ head strings) (tail strings)

mkDot :: PseudoExpr a => a -> String -> Expr
mkDot e1 attr = Dot (getExpr e1) attr

-- | Make an attribute access, i.e. self.<string>.
mkAttr :: String -> Expr
mkAttr s = mkName ("self." ++ s)

mkRelImport :: String -> Statement
mkRelImport name = FromImport "." name

mkAssign :: PseudoExpr a => a -> Expr -> Statement
mkAssign name expr = Assign (getExpr name) expr

mkIncr :: String -> Expr -> Statement
mkIncr name expr = AugmentedAssign (mkName name) Plus expr

mkCall :: PseudoExpr a => a -> [Expr] -> Expr
mkCall name args = Call (getExpr name) args

mkEnum :: String -> [(String, Expr)] -> Statement
mkEnum cname values =
  let body = map (uncurry mkAssign) values
  in Class cname [] body

mkXClass :: String -> String -> Bool -> Suite -> Suite -> Statement
mkXClass clazz superclazz False [] [] = mkEmptyClass clazz superclazz
mkXClass clazz superclazz xge constructor methods =
  let args = [ "self", "unpacker" ]
      super = mkCall (superclazz ++ ".__init__") $ map mkName args
      body = eventToUnpacker : (StmtExpr super) : constructor
      xgeexp = mkAssign "xge" (if xge then (mkName "True") else (mkName "False"))
      initMethod = Fun "__init__" args body
  in mkClass clazz superclazz $ xgeexp : initMethod : methods

    where

      -- In some cases (e.g. when creating ClientMessageEvents), our events are
      -- passed directly to __init__. Since we don't keep track of the
      -- underlying buffers after the event is created, we have to re-pack
      -- things so they can be unpacked again.
      eventToUnpacker :: Statement
      eventToUnpacker = let newUnpacker = mkAssign "unpacker" (mkCall "xcffib.MemoryUnpacker"
                                                              [mkCall "unpacker.pack" []])
                            cond = mkCall "isinstance" [mkName "unpacker", mkName "xcffib.Protobj"]
                        in mkIf cond [newUnpacker]


mkEmptyClass :: String -> String -> Statement
mkEmptyClass clazz superclazz = mkClass clazz superclazz [Pass]

mkClass :: String -> String -> Suite -> Statement
mkClass clazz superclazz body = Class clazz [superclazz] body

mkStr :: String -> Expr
mkStr s = Strings ["\"", s, "\""]

mkUnpackFrom :: PseudoExpr a => a -> [String] -> String -> Suite
mkUnpackFrom unpacker names packs =
  let lhs = Tuple $ map mkAttr names
      -- Don't spam with this default arg unless it is really necessary.
      unpackF = mkDot unpacker "unpack"
      rhs = mkCall unpackF [mkStr packs]
      stmt = if length names > 0 then mkAssign lhs rhs else StmtExpr rhs
  in if length packs > 0 then [stmt] else []

mkDict :: String -> Statement
mkDict name = mkAssign name EmptyDict

mkDictUpdate :: String -> Int -> String -> Statement
mkDictUpdate dict key value =
  mkAssign (Subscript (mkName dict) (Int key)) (mkName value)

mkMethod :: String -> [Ident] -> Suite -> Statement
mkMethod name args body = Fun name args body

mkReturn :: Expr -> Statement
mkReturn = Return . Just

mkIf :: Expr -> Suite -> Statement
mkIf e s = Conditional e s []

notImplemented :: Statement
notImplemented = Raise "xcffib.XcffibNotImplemented"

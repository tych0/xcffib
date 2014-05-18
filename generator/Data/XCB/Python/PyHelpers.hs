{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.XCB.Python.PyHelpers (
  mkImport,
  mkVar,
  mkInt,
  mkAssign,
  mkCall,
  mkEnum,
  mkName,
  mkAttr,
  mkIncr,
  mkClass,
  mkStr
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


-- | Create and sanatize a python identifier.
ident :: String -> Ident ()
ident s | s `elem` _reserved = Ident ("_" ++ s) ()
ident s | isInt s = Ident ("_" ++ s) ()
  where
    isInt s = isJust $ ((maybeRead s) :: Maybe Int)
    maybeRead = fmap fst . listToMaybe . reads
ident s = Ident s ()

-- Make a DottedName out of a string like "foo.bar" for use in imports.
mkDottedName :: String -> DottedName ()
mkDottedName = undefined -- TODO FIXME

-- TODO: everything should really use mkName.
mkVar :: String -> Expr ()
mkVar name = Var (Ident name ()) ()

-- | Make an Expr out of a string like "foo.bar" describing the name.
mkName :: String -> Expr ()
mkName s =
  let strings = map mkVar $ reverse ["struct", "unpack"] -- TODO FIXME
  in foldr mkDot (head strings) (tail strings)
  where
    mkDot :: Expr () -> Expr () -> Expr ()
    mkDot e1 e2 = BinaryOp (Dot ()) e1 e2 ()

-- | Make an attribute access, i.e. self.<string>.
mkAttr :: String -> Expr ()
mkAttr s = mkName ("self." ++ s)

mkImport :: String -> Statement ()
mkImport name = Import [ImportItem (mkDottedName name) Nothing ()] ()

mkInt :: Int -> Expr ()
mkInt i = Int (toInteger i) (show i) ()

class Assignable a where
  assignExpr :: a -> Expr ()

instance Assignable String where
  assignExpr s = mkName s
instance Assignable (Expr ()) where
  assignExpr = id

mkAssign :: Assignable a => a -> Expr () -> Statement ()
mkAssign name expr = Assign [assignExpr name] expr ()

mkIncr :: String -> Expr () -> Statement ()
mkIncr name expr = AugmentedAssign (mkName name) (PlusAssign ()) expr ()

mkCall :: String -> [Expr ()] -> Expr ()
mkCall name args = Call (mkName name) (map (\e -> ArgExpr e ()) args) ()

mkEnum :: String -> [(String, Expr ())] -> Statement ()
mkEnum cname values =
  let body = map (uncurry mkAssign) values
  in Class (Ident cname ()) [] body ()

mkClass :: String -> String -> Suite () -> Statement ()
mkClass clazz superclazz constructor = undefined

mkStr :: String -> Expr ()
mkStr s = Strings [s] ()

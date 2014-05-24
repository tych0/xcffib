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
  mkStr,
  mkUnpackFrom
  ) where

import Data.List
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
    isInt str = isJust $ ((maybeRead str) :: Maybe Int)
    maybeRead = fmap fst . listToMaybe . reads
ident s = Ident s ()

-- Make a DottedName out of a string like "foo.bar" for use in imports.
mkDottedName :: String -> DottedName ()
mkDottedName = map ident . splitOn "."

-- TODO: everything should really use mkName.
mkVar :: String -> Expr ()
mkVar name = Var (ident name) ()

-- | Make an Expr out of a string like "foo.bar" describing the name.
mkName :: String -> Expr ()
mkName s =
  let strings = map mkVar $ reverse $ splitOn "." s
  in foldr mkDot (head strings) (reverse $ tail strings)
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

mkParams :: [String] -> [Parameter ()]
mkParams = map (\x -> Param (ident x) Nothing Nothing ())

mkArg :: String -> Argument ()
mkArg n = ArgExpr (mkName n) ()

mkClass :: String -> String -> Suite () -> Statement ()
mkClass clazz superclazz constructor =
  -- TODO: Can we move size to a dynamically calculated property?
  let super = mkCall (superclazz ++ ".__init__") [ mkName "self"
                                                 , mkName "parent"
                                                 , mkName "offset"
                                                 , mkName "size"
                                                 ]
      body = [(StmtExpr super ())] ++ constructor
      initParams = mkParams ["self", "parent", "offset", "size"]
      initMethod = Fun (ident "__init__") initParams Nothing body ()
  in Class (ident clazz) [mkArg superclazz] [initMethod] ()

mkStr :: String -> Expr ()
mkStr s = Strings ["\"", s, "\""] ()

mkTuple :: [Expr ()] -> Expr ()
mkTuple = flip Tuple ()

mkUnpackFrom :: [String] -> [String] -> Statement ()
mkUnpackFrom names packs =
  let lhs = mkTuple $ map mkAttr names
      unpackStr = mkStr $ intercalate "" packs
      rhs = mkCall "struct.unpack_from" [ unpackStr
                                        , mkName "parent"
                                        , mkName "offset"
                                        ]
  in mkAssign lhs rhs

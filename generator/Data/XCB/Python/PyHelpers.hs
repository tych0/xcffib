module Data.XCB.Python.PyHelpers (
  mkImport,
  mkVar,
  mkInt,
  mkAssign,
  mkCall,
  mkArg)
  where

import Language.Python.Common

-- Make a DottedName out of a string like "foo.bar" for use in imports.
mkDottedName :: String -> DottedName ()
mkDottedName = undefined -- TODO FIXME

mkVar :: String -> Expr ()
mkVar name = Var (Ident name ()) ()

-- Make an Expr out of a string like "foo.bar" describing the name.
mkName :: String -> Expr ()
mkName s =
  let strings = map mkVar $ reverse ["struct", "unpack"] -- TODO FIXME
  in foldr mkDot (head strings) (tail strings)
  where
    mkDot :: Expr () -> Expr () -> Expr ()
    mkDot e1 e2 = BinaryOp (Dot ()) e1 e2 ()

mkImport :: String -> Statement ()
mkImport name = Import [ImportItem (mkDottedName name) Nothing ()] ()

mkInt :: Int -> Expr ()
mkInt i = Int (toInteger i) (show i) ()

mkAssign :: String -> Expr () -> Statement ()
mkAssign name expr = Assign [mkName name] expr ()

mkCall :: String -> [Argument ()] -> Expr ()
mkCall name args = Call (mkName name) args ()

mkArg :: Expr () -> Argument ()
mkArg e = ArgExpr e ()

mkEnum :: String -> [(String, Int)] -> Statement ()
mkEnum cname values = undefined

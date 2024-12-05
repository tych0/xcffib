{-
 - Copyright 2024 Tycho Andersen
 -
 - This module exists as a small AST to replace language-python, which seems
 - largely unmaintained at this point. This is not really intended to be a
 - complete python grammer, mostly it is just enough to be what xcffib needs to
 - generate its trees.
 -
 - It has no annotations as in language-python, because xcffib doesn't use them.
 -
 - The grammar not complete: it cannot express anything other than empty
 - dictionaries, cannot express sets at all, cannot do async, etc. all because
 - these features of the language are unused by xcffib.
 -
 - The grammar is not sound: it has one "op" class, representing both unary and
 - binary operators.
 -
 - Use at your own risk :)
 -}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.XCB.Python.AST (
  Expr(..),
  Suite,
  Statement(..),
  Ident,
  Op(..),
  Pretty(..),
  prettyText,
  PseudoExpr(..)
) where

import Prelude hiding ((<>))

import Data.Maybe

import Text.PrettyPrint

type Suite = [Statement]

type Ident = String

-- we don't use that many operations :)
data Op
   = Plus
   | Minus
   | Multiply
   | FloorDivide
   | BinaryAnd
   | ShiftRight
   | ShiftLeft
   | Invert
   | Equality
   | LessThan
   | Modulo
   deriving (Eq, Ord, Show)

data Statement
    = Import
      { import_item :: Ident }
    -- this is only a one level dotted import; other levels are not supported
    | FromImport
      { from_module :: Ident
      , from_item :: Ident
      }
    -- we skip While, For, AsyncFor, etc. because we don't use them
    | Fun
      { fun_name :: Ident
      -- only named variables, no default values, kwargs, etc.
      , fun_args :: [Ident]
      , fun_body :: Suite
      }
    | Decorated
      { decorations :: Ident
      -- only decorated functions/methods are supported
      , fun_name :: Ident
      , fun_args :: [Ident]
      , fun_body :: Suite
      }
    -- skip AsyncFun...
    | Class
      { class_name :: Ident
      -- same as functions, only identifiers allowed
      , class_args :: [Ident]
      , class_body :: Suite
      }
    | Conditional
      { if_cond :: Expr
      , if_body :: Suite
      , else_body :: Suite
      }
    | Assign
      { assign_to :: Expr
      , assign_expr :: Expr
      }
    | AugmentedAssign
      { aug_assign_to :: Expr
      , aug_assign_op :: Op
      , aug_assign_expr :: Expr
      }
    -- skip AnnotatedAssign, Decorated
    | Return
      { return_expr :: Maybe (Expr) }
    -- skip Try, With, AsyncWith
    | Pass {}
    -- skip Break, Continue, Delete
    | StmtExpr
      { stmt_expr :: Expr }
    -- skip Global, NonLocal, Assert, Print, Exec
    | Raise { raise_exception :: Ident }
    deriving (Eq, Ord, Show)

data Expr
    = Var { var :: Ident }
    | Int { int_value :: Int }
    | Bool { bool_value :: Bool }
    | None
    | Strings { strings_strings :: [String] }
    | Call
      { call_fun :: Expr
      , call_args :: [Expr]
      }
    | CondExpr
      { ce_true_branch :: Expr
      , ce_conditon :: Expr
      , ce_false_branch :: Expr
      }
    | Subscript
      { subscriptee :: Expr
      , subscript_expr :: Expr
      }
    | BinaryOp
      { operator :: Op
      , binop_left :: Expr
      , binop_right :: Expr
      }
    | UnaryOp
      { operator :: Op
      , unop_arg :: Expr
      }
    | Dot
      { dot_expr :: Expr
      , dot_attribute :: Ident
      }
    | Tuple { tuple_exprs :: [Expr] }
    | EmptyDict {} -- an empty dictionary
    | Paren { paren_expr :: Expr }
    deriving (Eq, Ord, Show)

prettyText :: Pretty a => a -> String
prettyText = render . pretty

class Pretty a where
    pretty :: a -> Doc

instance Pretty Op where
    pretty Plus = text "+"
    pretty Minus = text "-"
    pretty Multiply = text "*"
    pretty FloorDivide = text "//"
    pretty BinaryAnd = text "&"
    pretty ShiftRight = text ">>"
    pretty ShiftLeft = text "<<"
    pretty Invert = text "~"
    pretty Equality = text "=="
    pretty LessThan = text "<"
    pretty Modulo = text "%"

_reserved :: [String]
_reserved = [ "None"
            , "def"
            , "class"
            , "and"
            , "or"
            ]

-- | Sanitize identifiers.
instance Pretty Ident where
    pretty s | s `elem` _reserved = text $ "_" ++ s
    pretty s | isInt s = text $ "_" ++ s
      where
        isInt str = isJust $ ((maybeRead str) :: Maybe Int)
        maybeRead = fmap fst . listToMaybe . reads
    pretty s = text s

instance Pretty Suite where
    pretty stmts = vcat $ map pretty stmts

indent :: Doc -> Doc
indent = nest 4

instance Pretty Statement where
    pretty (Import item) = text "import" <+> pretty item
    pretty (FromImport source item) = text "from" <+> text source <+> text "import" <+> pretty item
    pretty (Fun name args bod) = text "def" <+> pretty name <> (parens (addCommas args)) <> colon
                                 $+$ indent (pretty bod)
    pretty (Decorated decorator name args bod) = text "@" <> text decorator $+$ pretty (Fun name args bod)
    pretty (Class name [] body) = text "class" <+> pretty name <> colon $+$ indent (pretty body)
    pretty (Class name superclasses body) = text "class" <+> pretty name <> parens (addCommas superclasses) <> colon $+$ indent (pretty body)
    pretty (Conditional cond if_ else_) = text "if" <+> pretty cond <> colon $+$ indent (pretty if_) $+$ pretty else_
    pretty (Assign to expr) = pretty to <+> text "=" <+> pretty expr
    pretty (AugmentedAssign to op expr) = pretty to <+> pretty op <> text "=" <+> pretty expr
    pretty (Return (Just expr)) = text "return" <+> pretty expr
    pretty (Return Nothing) = text "return"
    pretty Pass = text "pass"
    pretty (StmtExpr expr) = pretty expr
    pretty (Raise exc) = text "raise" <+> pretty exc

class PseudoExpr a where
  getExpr :: a -> Expr

instance PseudoExpr String where
  getExpr s = Var s
instance PseudoExpr Expr where
  getExpr = id

addCommas :: PseudoExpr a => [a] -> Doc
addCommas exprs = hsep $ punctuate (text ",") (map (pretty . getExpr) exprs)

instance Pretty Expr where
    pretty (Var v) = pretty v
    pretty (Int i) = integer (toInteger i)
    pretty (Bool True) = text "True"
    pretty (Bool False) = text "False"
    pretty None = text "None"
    pretty (Strings xs) = hcat (map text xs)
    pretty (Call fun args) = pretty fun <> lparen <> addCommas args <> rparen
    pretty (CondExpr trueB cond falseB) = pretty trueB <+> text "if" <+> pretty cond <+> text "else" <+> pretty falseB
    pretty (Subscript thing expr) = pretty thing <> brackets (pretty expr)
    pretty (BinaryOp op left right) = pretty left <+> pretty op <+> pretty right
    pretty (UnaryOp op arg) = pretty op <> pretty arg
    pretty (Dot thing attr) = pretty thing <> text "." <> pretty attr
    pretty (Tuple [expr]) = pretty expr <> comma -- one element still needs to be a tuple
    pretty (Tuple exprs) = addCommas exprs
    pretty EmptyDict = text "{}"
    pretty (Paren expr) = lparen <> pretty expr <> rparen

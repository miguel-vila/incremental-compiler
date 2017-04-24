module Expr where

data Expr = FixNum Integer
          | Boolean Bool
          | Character Char
          | Nil
          | FnApp String [Expr]
          | If Expr Expr Expr
          | And [Expr]
          | Or [Expr]
          | NoOp

module Expr where

data Expr = FixNum Integer
          | Boolean Bool
          | Character Char
          | Nil
          | UnaryFnApp String Expr

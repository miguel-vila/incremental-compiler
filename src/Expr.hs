module Expr where

data Expr = L Literal
          | FnApp String [Expr]
          | If Expr Expr Expr
          | And [Expr]
          | Or [Expr]
          | Let [Binding] Expr
          | NoOp

data Literal = FixNum Integer
             | Boolean Bool
             | Character Char
             | Nil

data Binding = Binding { name :: String
                       , value :: Expr
                       }

_False :: Expr
_False = L $ Boolean False

_True :: Expr
_True = L$ Boolean True

fx :: Integer -> Expr
fx = L . FixNum

char :: Char -> Expr
char = L . Character

nil :: Expr
nil = L Nil

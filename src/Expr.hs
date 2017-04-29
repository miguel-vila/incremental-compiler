module Expr where

data Expr = L Literal
          | FnApp String [Expr]
          | If Expr Expr Expr
          | And [Expr]
          | Or [Expr]
          | Let     [Binding] Expr
          | LetStar [Binding] Expr
          | VarRef VarName
          | NoOp

data Literal = FixNum Integer
             | Boolean Bool
             | Character Char
             | Nil

type VarName = String

data Binding = Binding { name :: VarName
                       , expr :: Expr
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

var :: String -> Expr
var = VarRef

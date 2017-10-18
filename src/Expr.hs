{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr where

import Text.Show.Deriving
import Data.Eq.Deriving
import Data.Functor.Foldable hiding (Nil)

type ParamName = String

type FunctionName = String

data ExprF a = L Literal
             | PrimitiveApp FunctionName [a]
             | If a a a
             | And [a]
             | Or [a]
             | Let     [BindingF a] a
             | LetStar [BindingF a] a
             | VarRef VarName
             | NoOp
             | UserFnApp FunctionName [a]
             | Do [a]
             deriving (Show, Eq, Functor)

data Literal = FixNum Integer
             | Boolean Bool
             | Character Char
             | Nil
             deriving (Show, Eq)

type VarName = String

type FnName = String

type BindingF a = (VarName, a)

type Binding = BindingF Expr

type Expr = Fix ExprF

$(deriveShow1 ''ExprF)

$(deriveEq1 ''ExprF)

data Lambda = Lambda { params :: [ParamName]
                     , body   :: Expr
                     } deriving (Show, Eq)

data LambdaBinding = LambdaBinding { functionName :: FnName
                                   , lambda       :: Lambda
                                   } deriving (Show, Eq)

data Program = Expr Expr
             | LetRec [LambdaBinding] Expr
             deriving (Show, Eq)

_False :: Expr
_False = Fix $ L $ Boolean False

_True :: Expr
_True = Fix $ L $ Boolean True

fx :: Integer -> Expr
fx = Fix . L . FixNum

char :: Char -> Expr
char = Fix . L . Character

nil :: Expr
nil = Fix $ L Nil

var :: String -> Expr
var = Fix . VarRef

literal :: Literal -> Expr
literal = Fix . L

primitiveApp :: FunctionName -> [Expr] -> Expr
primitiveApp fnName args =
  Fix $ PrimitiveApp fnName args

userFnApp :: FunctionName -> [Expr] -> Expr
userFnApp fnName args =
  Fix $ UserFnApp fnName args

_if :: Expr -> Expr -> Expr -> Expr
_if cond conseq altern =
  Fix $ If cond conseq altern

_and :: [Expr] -> Expr
_and = Fix . And

_or :: [Expr] -> Expr
_or = Fix . Or

_let :: [Binding] -> Expr -> Expr
_let bindings body = Fix $ Let bindings body

_letStar :: [Binding] -> Expr -> Expr
_letStar bindings body = Fix $ LetStar bindings body

_do :: [Expr] -> Expr
_do = Fix . Do

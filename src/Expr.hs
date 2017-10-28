{-# LANGUAGE DeriveFunctor
           , FlexibleContexts
           , FlexibleInstances
           , TemplateHaskell
           , TypeOperators
           , MultiParamTypeClasses
           #-}

module Expr where

import Text.Show.Deriving
import Data.Eq.Deriving
import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Sum

infixr 6 :+:
type (:+:) = Sum

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

inject' :: f :<: g => f (Fix g) -> Fix g
inject' = Fix . inj

project' :: f :<: g => Fix g -> Maybe (f (Fix g))
project' (Fix f) = prj f

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

  prj (InL fa) = Just fa
  prj _        = Nothing

instance (Functor f, Functor g, Functor h, f :<: h) => f :<: (g :+: h) where
  inj = InR . inj

  prj (InR ha) = prj ha
  prj _        = Nothing

type ParamName = String

type FunctionName = String

data AndF a = And [a]
              deriving (Show, Eq, Functor)

$(deriveEq1   ''AndF)
$(deriveShow1 ''AndF)

data OrF a = Or [a]
             deriving (Show, Eq, Functor)

$(deriveEq1   ''OrF)
$(deriveShow1 ''OrF)

data ExprF a = L Literal
             | PrimitiveApp FunctionName [a]
             | If a a a
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

binding :: VarName -> a -> BindingF a
binding = (,)

type Expr = Fix ExprF

type ParsedExprF = AndF :+: OrF :+: ExprF

type ParsedExpr = Fix ParsedExprF

$(deriveShow1 ''ExprF)

$(deriveEq1 ''ExprF)

data LambdaF a = Lambda { params :: [ParamName]
                        , body   :: a
                        } deriving (Show, Eq, Functor)

type Lambda = LambdaF Expr

data LambdaBindingF a = LambdaBinding { functionName :: FnName
                                      , lambda       :: LambdaF a
                                      } deriving (Show, Eq, Functor)

type LambdaBinding = LambdaBindingF Expr

data Program = Expr ParsedExpr
             | LetRec [LambdaBindingF ParsedExpr] ParsedExpr
             deriving (Show, Eq)

_False :: ExprF :<: f => Fix f
_False = inject' $ L $ Boolean False

_True :: ExprF :<: f => Fix f
_True = inject' $ L $ Boolean True

fx :: ExprF :<: f => Integer -> Fix f
fx = inject' . L . FixNum

char :: ExprF :<: f => Char -> Fix f
char = inject' . L . Character

nil :: ExprF :<: f => Fix f
nil = inject' $ L Nil

var :: ExprF :<: f => String -> Fix f
var = inject' . VarRef

literal :: ExprF :<: f => Literal -> Fix f
literal = inject' . L

primitiveApp :: ExprF :<: f => FunctionName -> [Fix f] -> Fix f
primitiveApp fnName args =
  inject' $ PrimitiveApp fnName args

userFnApp :: ExprF :<: f => FunctionName -> [Fix f] -> Fix f
userFnApp fnName args =
  inject' $ UserFnApp fnName args

_if :: ExprF :<: f => Fix f -> Fix f -> Fix f -> Fix f
_if cond conseq altern =
  inject' $ If cond conseq altern

_and :: AndF :<: f => [Fix f] -> Fix f
_and = inject' . And

_or :: OrF :<: f => [Fix f] -> Fix f
_or = inject' . Or

_let :: ExprF :<: f => [BindingF (Fix f)] -> Fix f -> Fix f
_let bindings letBody = inject' $ Let bindings letBody

_letStar :: ExprF :<: f => [BindingF (Fix f)] -> Fix f -> Fix f
_letStar bindings letBody = inject' $ LetStar bindings letBody

_do :: ExprF :<: f => [Fix f] -> Fix f
_do = inject' . Do

{-# LANGUAGE FlexibleContexts
           , TypeOperators
           #-}
module Desugaring where

import Expr
import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Sum

expandAnd :: ExprF :<: f => (AndF :+: f) (Fix f) -> f (Fix f)
expandAnd (InR expr)       = expr
expandAnd (InL (And args)) = expand args
  where false :: ExprF :<: f => Fix f
        false = Fix $ inj $ L $ Boolean False
        expand :: ExprF :<: f => [Fix f] -> f (Fix f)
        expand []          = inj $ L $ Boolean False
        expand [test]      = unfix test
        expand (test:rest) = inj $ If test (Fix $ expand rest) false

expandOr :: ExprF :<: f => (OrF :+: f) (Fix f) -> f (Fix f)
expandOr (InR expr)       = expr
expandOr (InL (Or args)) = expand args
  where noop :: ExprF :<: f => Fix f
        noop = Fix $ inj $ NoOp
        expand :: ExprF :<: f => [Fix f] -> f (Fix f)
        expand []          = inj $ L $ Boolean True
        expand [test]      = unfix test
        expand (test:rest) = inj $ If test noop (Fix $ expand rest)

transCata :: (Functor f, Functor g) => (g (Fix f) -> f (Fix f)) -> Fix g -> Fix f
transCata f = cata (embed . f)

desugarAnd :: ExprF :<: f => Fix (AndF :+: f) -> Fix f
desugarAnd = transCata expandAnd

desugarOr :: ExprF :<: f => Fix (OrF :+: f) -> Fix f
desugarOr = transCata expandOr

desugar :: ExprF :<: f => Fix (AndF :+: OrF :+: f) -> Fix f
desugar = desugarOr . desugarAnd

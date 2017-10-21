{-# LANGUAGE FlexibleContexts
           , TypeOperators
           #-}

module TestUtils where

import Expr
import Data.Functor.Foldable hiding (Nil)

binOp :: ExprF :<: f => String -> Fix f -> Fix f -> Fix f
binOp name arg1 arg2 = primitiveApp name [arg1, arg2]

unaryOp :: ExprF :<: f => String -> Fix f -> Fix f
unaryOp name arg = primitiveApp name [arg]

app :: ExprF :<: f => FunctionName -> Fix f -> Fix f
app f arg = userFnApp f [arg]

binApp :: ExprF :<: f => FunctionName -> Fix f -> Fix f -> Fix f
binApp f arg1 arg2 = userFnApp f [arg1, arg2]

sumFirstN1 :: ExprF :<: f => LambdaBindingF (Fix f)
sumFirstN1 =
  LambdaBinding "sum" (Lambda ["n", "acc"]
                        (_if (unaryOp "fxzero?" (var "n"))
                          (var "acc")
                          (userFnApp "sum" [ unaryOp "fxsub1" (var "n")
                                           , binOp "fx+" (var "n") (var "acc")])))

evenOddBindings :: ExprF :<: f => [LambdaBindingF (Fix f)]
evenOddBindings =
  [ LambdaBinding "e" $ Lambda ["x"]
    (_if (unaryOp "fxzero?" (var "x")) _True (app "o" (unaryOp "fxsub1" (var "x"))))
  , LambdaBinding "o" $ Lambda ["x"]
    (_if (unaryOp "fxzero?" (var "x")) _False (app "e" (unaryOp "fxsub1" (var "x"))))
  ]

cons :: ExprF :<: f => Fix f -> Fix f -> Fix f
cons = binOp "cons"

car :: ExprF :<: f => Fix f -> Fix f
car = unaryOp "car"

cdr :: ExprF :<: f => Fix f -> Fix f
cdr = unaryOp "cdr"

vector :: ExprF :<: f => Fix f -> Fix f -> Fix f
vector = binOp "make-vector"

vectorRef :: ExprF :<: f => Fix f -> Fix f -> Fix f
vectorRef = binOp "vector-ref"

vectorSet :: ExprF :<: f => Fix f -> Fix f -> Fix f -> Fix f
vectorSet vector position value =
  primitiveApp "vector-set!" [vector, position, value]

(~>) = (,)
(<~) = (,)

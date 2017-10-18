module TestUtils where

import Expr

binOp :: String -> Expr -> Expr -> Expr
binOp name arg1 arg2 = primitiveApp name [arg1, arg2]

unaryOp :: String -> Expr -> Expr
unaryOp name arg = primitiveApp name [arg]

app :: FunctionName -> Expr -> Expr
app f arg = userFnApp f [arg]

binApp :: FunctionName -> Expr -> Expr -> Expr
binApp f arg1 arg2 = userFnApp f [arg1, arg2]

sumFirstN1 :: LambdaBinding
sumFirstN1 =
  LambdaBinding "sum" (Lambda ["n", "acc"]
                        (_if (unaryOp "fxzero?" (var "n"))
                          (var "acc")
                          (userFnApp "sum" [ unaryOp "fxsub1" (var "n")
                                           , binOp "fx+" (var "n") (var "acc")])))

evenOddBindings :: [LambdaBinding]
evenOddBindings =
  [ LambdaBinding "e" $ Lambda ["x"]
    (_if (unaryOp "fxzero?" (var "x")) _True (app "o" (unaryOp "fxsub1" (var "x"))))
  , LambdaBinding "o" $ Lambda ["x"]
    (_if (unaryOp "fxzero?" (var "x")) _False (app "e" (unaryOp "fxsub1" (var "x"))))
  ]

cons :: Expr -> Expr -> Expr
cons = binOp "cons"

car :: Expr -> Expr
car = unaryOp "car"

cdr :: Expr -> Expr
cdr = unaryOp "cdr"

vector :: Expr -> Expr -> Expr
vector = binOp "make-vector"

vectorRef :: Expr -> Expr -> Expr
vectorRef = binOp "vector-ref"

vectorSet :: Expr -> Expr -> Expr -> Expr
vectorSet vector position value =
  primitiveApp "vector-set!" [vector, position, value]

(~>) = (,)
(<~) = (,)

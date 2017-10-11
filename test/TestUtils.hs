module TestUtils where

import Expr

binOp :: String -> Expr -> Expr -> Expr
binOp name arg1 arg2 = PrimitiveApp name [arg1, arg2]

unaryOp :: String -> Expr -> Expr
unaryOp name arg = PrimitiveApp name [arg]

app :: FunctionName -> Expr -> Expr
app f arg = UserFnApp f [arg]

binApp :: FunctionName -> Expr -> Expr -> Expr
binApp f arg1 arg2 = UserFnApp f [arg1, arg2]

sumFirstN1 :: LambdaBinding
sumFirstN1 =
  LambdaBinding "sum" (Lambda ["n", "acc"]
                        (If (unaryOp "fxzero?" (var "n"))
                          (var "acc")
                          (UserFnApp "sum" [ unaryOp "fxsub1" (var "n")
                                           , binOp "fx+" (var "n") (var "acc")])))

evenOddBindings :: [LambdaBinding]
evenOddBindings =
  [ LambdaBinding "e" $ Lambda ["x"]
    (If (unaryOp "fxzero?" (var "x")) _True (app "o" (unaryOp "fxsub1" (var "x"))))
  , LambdaBinding "o" $ Lambda ["x"]
    (If (unaryOp "fxzero?" (var "x")) _False (app "e" (unaryOp "fxsub1" (var "x"))))
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

_let :: [(String, Expr)] -> Expr -> Expr
_let bindings body =
  Let (map (\(name,expr) -> Binding name expr) bindings) body

_letStar :: [(String, Expr)] -> Expr -> Expr
_letStar bindings body =
  LetStar (map (\(name,expr) -> Binding name expr) bindings) body

vectorSet :: Expr -> Expr -> Expr -> Expr
vectorSet vector position value =
  PrimitiveApp "vector-set!" [vector, position, value]

(~>) = (,)
(<~) = (,)

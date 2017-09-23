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

(~>) = (,)

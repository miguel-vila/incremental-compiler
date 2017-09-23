module TestUtils where

import Expr

binOp :: String -> Expr -> Expr -> Expr
binOp name arg1 arg2 = FnApp name [arg1, arg2]

unaryOp :: String -> Expr -> Expr
unaryOp name arg = FnApp name [arg]

(~>) = (,)

module Main where

import Lib
import Expr


binOp op arg1 arg2 = FnApp op [arg1, arg2]

main :: IO ()
main = do
  let --add2 = LambdaBinding "add2" $ Lambda ["x"] (FnApp "fx+" [fx 2, VarRef "x"])
      --source = LetRec [add2] (UserFnApp "add2" [fx 7])
      --let source = FixNum $ -1
      --output <- compileAndExecute source
      --source = Expr $ FnApp "fx+" [ FnApp "fx+" [(fx 3), (fx 5)], FnApp "fx+" [(fx 7), (fx 12)] ]
      source = Expr $ Let [Binding "x" (fx 1)] (Let [Binding "x" (binOp "fx+" (var "x") (fx 1)), Binding "y" (binOp "fx+" (var "x") (fx 1))] (var "y"))
      output = compileCode source
    in putStrLn output

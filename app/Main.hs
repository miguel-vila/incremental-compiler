module Main where

import Lib
import Expr

main :: IO ()
main = do
  let add2 = LambdaBinding "add2" $ Lambda ["x"] (FnApp "fx+" [fx 2, VarRef "x"])
      source = LetRec [add2] (UserFnApp "add2" [fx 7])
      --let source = FixNum $ -1
      --output <- compileAndExecute source
      output = compileCode source
    in putStrLn output

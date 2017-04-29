module Main where

import Lib
import Expr

main :: IO ()
main = do
  let source = Let [Binding "x" (fx 5), Binding "y" (fx 7)] (FnApp "fx+" [(var "x"), (var "y")])
  --let source = FixNum $ -1
  --output <- compileAndExecute source
  let output = compileCode source
  putStrLn output

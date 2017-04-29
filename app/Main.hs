module Main where

import Lib
import Expr

main :: IO ()
main = do
  let source = FnApp "fx-" [fx 5, fx 1]
  --let source = FixNum $ -1
  --output <- compileAndExecute source
  let output = compileCode source
  putStrLn output

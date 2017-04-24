module Main where

import Lib
import Expr

main :: IO ()
main = do
  let source = FnApp "fxlogand" [(FixNum 0), (FixNum 1)]
  --let source = FixNum $ -1
  --output <- compileAndExecute source
  let output = compileCode source
  putStrLn output

module Main where

import Lib
import Expr

main :: IO ()
main = do
  let source = BinaryFnApp "fx+" (BinaryFnApp "fx+" (FixNum 3) (FixNum 6)) (BinaryFnApp "fx+" (FixNum 1) (FixNum 0))
  --let source = FixNum $ -1
  --output <- compileAndExecute source
  let output = compileCode source
  putStrLn output

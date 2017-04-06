module Main where

import Lib
import Expr

main :: IO ()
main = do
  output <- compileAndExecute (UnaryFnApp "char->fixnum" (Character '0'))
  putStrLn output

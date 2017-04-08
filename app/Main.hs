module Main where

import Lib
import Expr

main :: IO ()
main = do
  output <- compileAndExecute (UnaryFnApp "fixnum?" (FixNum 3))
  putStrLn output

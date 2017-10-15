module Lib
    ( compileAndExecute
    , compileCode
    ) where

import System.Process
import System.IO
import System.Directory
--import Parser
import Expr
import Emitter

compileAndExecute :: Program -> IO String
compileAndExecute source = do
  let programText = compileCode source
  writeFile "tmp-program.s" programText
  let command = "gcc -m32 -o tmp-program runtime/runtime.c tmp-program.s"
  callCommand command
  (_,handle,_,processHandle) <- runInteractiveCommand "./tmp-program"
  exitCode <- waitForProcess processHandle
  output <- hGetLine handle
  mapM_ removeFile ["tmp-program.s", "tmp-program"]
  return output

compileCode :: Program -> String
compileCode program =
  let Right(lines) = compile program
  in unlines lines

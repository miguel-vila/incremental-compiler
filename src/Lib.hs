module Lib
    ( compileAndExecute
    ) where

import System.Process
import System.IO
import System.Directory
--import Parser
import Expr
import Emitter

compileAndExecute :: Expr -> IO String
compileAndExecute source = do
  let program = compile source
  let programText = unlines program
  writeFile "tmp-program.s" programText
  let command = "gcc -o tmp-program runtime.c tmp-program.s"
  callCommand command
  (_,handle,_,processHandle) <- runInteractiveCommand "./tmp-program"
  exitCode <- waitForProcess processHandle
  output <- hGetLine handle
  mapM_ removeFile ["tmp-program.s", "tmp-program"]
  return output

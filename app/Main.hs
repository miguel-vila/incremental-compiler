module Main where

import Lib
import Expr
import Parser
import Emitter
import System.Environment(getArgs)
import System.IO
import System.Process
import System.Exit
import Data.String.Utils

printError :: String -> IO ()
printError errorMessage =
  hPutStrLn stderr errorMessage

printErrorAndExit :: String -> IO a
printErrorAndExit errorMessage =
  do printError errorMessage
     exitFailure

getFileName :: [String] -> IO String
getFileName []             =
  printErrorAndExit "Must pass filename"
getFileName (filePath : _) =
  return filePath

parse :: String -> IO Program
parse fileContent =
  case readProgram fileContent of
    Left parseError ->
      do printError "Error while parsing"
         printError $ "Position: " ++ (position parseError)
         mapM_ printError (messages parseError)
         exitFailure
    Right ast ->
      return ast

removeFormat :: String -> String
removeFormat sourceFileName =
  (concat $ init (split "." sourceFileName))

compileGCC :: String -> String -> IO ()
compileGCC sourceFileName outputFilename =
  callCommand $ "gcc -m32 -g -o " ++ outputFilename ++ " runtime/runtime.c " ++ sourceFileName

main :: IO ()
main = do
  do args <- getArgs
     fileName <- getFileName args
     fileContent <- readFile fileName
     ast <- parse fileContent
     let compiledCode = unlines $ compile ast
     let fileNameWithoutFormat = removeFormat fileName
     let compiledCodeFileName = fileNameWithoutFormat ++ ".s"
     writeFile compiledCodeFileName compiledCode
     compileGCC compiledCodeFileName fileNameWithoutFormat
     putStrLn $ "Successful compilation! You can see the file: " ++ fileNameWithoutFormat

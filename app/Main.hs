module Main where

import Lib
import Expr
import Parser
import Emitter
import System.Environment(getArgs)
import System.IO
import System.Exit
import Text.ParserCombinators.Parsec.Error
import Data.String.Utils

binOp op arg1 arg2 = PrimitiveApp op [arg1, arg2]

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
         printError $ "Position: " ++ (show $ errorPos parseError)
         mapM_ (printError . messageString) (errorMessages parseError)
         exitFailure
    Right ast ->
      return ast

replaceFormatWithS :: String -> String
replaceFormatWithS sourceFileName =
  (concat $ init (split "." sourceFileName)) ++ ".s"

main :: IO ()
main = do
  do args <- getArgs
     fileName <- getFileName args
     fileContent <- readFile fileName
     ast <- parse fileContent
     let compiledCode = unlines $ compile ast
     let compiledCodeFileName = replaceFormatWithS fileName
     writeFile compiledCodeFileName compiledCode
     putStrLn $ "Successful compilation! You can see the file: " ++ compiledCodeFileName

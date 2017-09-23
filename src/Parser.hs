module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.HashMap hiding (map)
import Data.Char (isSpace)
import Control.Monad.State.Lazy
import Control.Monad.Except
import Numeric (readFloat)
import Expr hiding (char)
import Emitter

readOrThrow :: Parser a -> String -> Either ParseError a
readOrThrow parser input = parse parser "lisp" input

readExpr :: String -> Either ParseError Expr
readExpr = readOrThrow parseExpr

parseExpr :: Parser Expr
parseExpr =
  (L <$> parseLiteral) <|>
  parseIf <|>
  parseAndOr <|>
  parseLetOrLetStar <|>
  parseVarRef <|>
  parseFnApp

parseLiteral :: Parser Literal
parseLiteral = parseFixNum <|>
               parseBoolean <|>
               parseChar <|>
               parseNull

parseFixNum :: Parser Literal
parseFixNum = do
  s <- getInput
  case readFloat s :: [(Float, String)] of
    [(n,s')] -> (FixNum $ floor n) <$ setInput s' -- @TODO how to avoid the `floor`?
    _ -> fail "Not an integer"

parseBoolean :: Parser Literal
parseBoolean =
  let true = Boolean True <$ string "#t"
      false = Boolean False <$ string "#f"
  in true <|> false

parseChar :: Parser Literal
parseChar = Character <$> do
  char '#'
  anyChar

parseNull :: Parser Literal
parseNull = Nil <$ (string "null" <|> string "nil" <|> string "()")

isPrimitive :: FnName -> Bool
isPrimitive fnName = member fnName primitives

startList :: Parser Char
startList = char '('

endList :: Parser Char
endList = char ')'

atLeastOneSpace = many1 space

parseFnApp :: Parser Expr
parseFnApp = do
  startList
  atLeastOneSpace
  fnName <- parseVarName
  atLeastOneSpace
  args <- parseExpr `sepBy` atLeastOneSpace
  endList
  let constr = if isPrimitive fnName
               then FnApp
               else UserFnApp
  return $ constr fnName args

parseIf :: Parser Expr
parseIf = do
  startList
  string "if"
  atLeastOneSpace
  cond <- parseExpr
  atLeastOneSpace
  conseq <- parseExpr
  atLeastOneSpace
  altern <- parseExpr
  atLeastOneSpace
  endList
  return $ If cond conseq altern

parseListExpr :: Parser [Expr]
parseListExpr =
  parseExpr `sepBy` atLeastOneSpace

parseAndOr :: Parser Expr
parseAndOr = do
  startList
  which <- (string "and") <|> (string "or")
  atLeastOneSpace
  args <- parseListExpr
  spaces
  endList
  let constr = if which == "and" then And else Or
  return $ constr args

parseVarName :: Parser VarName
parseVarName = many1 (satisfy (\c -> not $ isSpace c))

parseBinding :: Parser Binding
parseBinding = do
  startList
  varName <- parseVarName
  atLeastOneSpace
  expr <- parseExpr
  spaces
  endList
  return $ Binding varName expr

parseLetOrLetStar :: Parser Expr
parseLetOrLetStar = do
  startList
  which <- (string "let") <|> (string "let*")
  atLeastOneSpace
  startList
  bindings <- parseBinding `sepBy` atLeastOneSpace
  endList
  body <- parseExpr
  endList
  let constr = if which == "let" then Let else LetStar
  return $ constr bindings body

parseVarRef :: Parser Expr
parseVarRef = VarRef <$> parseVarName

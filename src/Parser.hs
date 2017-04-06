module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Error
import Control.Monad.Except
import Numeric (readFloat)
import Expr

readOrThrow :: Parser a -> String -> Either ParseError a
readOrThrow parser input = parse parser "lisp" input

readExpr :: String -> Either ParseError Expr
readExpr = readOrThrow parseExpr

parseExpr :: Parser Expr
parseExpr =
  parseFixNum <|>
  parseBoolean <|>
  parseChar <|>
  parseNull

parseFixNum :: Parser Expr
parseFixNum = do
  s <- getInput
  case readFloat s :: [(Float, String)] of
    [(n,s')] -> (FixNum $ floor n) <$ setInput s' -- @TODO how to avoid the `floor`?
    _ -> fail "Not an integer"

parseBoolean :: Parser Expr
parseBoolean =
  let true = Boolean True <$ string "#t"
      false = Boolean False <$ string "#f"
  in true <|> false

parseChar :: Parser Expr
parseChar = Character <$> do
  char '#'
  anyChar

parseNull :: Parser Expr
parseNull = Nil <$ (string "null" <|> string "nil" <|> string "()")

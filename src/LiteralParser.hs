module LiteralParser where

import Expr hiding (char)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parseFixNum :: Parser Literal
parseFixNum = FixNum <$> int

parseStartingWithHash :: Parser Literal
parseStartingWithHash = do
  char '#'
  parseBoolean <|> parseChar

parseBoolean :: Parser Literal
parseBoolean =
  let true = Boolean True <$ char 't'
      false = Boolean False <$ char 'f'
  in true <|> false

parseChar :: Parser Literal
parseChar = Character <$> do
  char '\\'
  anyChar

nullOrNil :: Parser String
nullOrNil = do
  char 'n'
  string "ull" <|> string "il"

parseNull :: Parser Literal
parseNull = Nil <$ (nullOrNil <|> string "()")
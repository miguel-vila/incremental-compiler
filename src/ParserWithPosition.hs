module ParserWithPosition where

import Expr hiding (char)
import Parser
import Primitives
import LiteralParser

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Control.Comonad.Cofree

readExpr' :: String -> Either ParsingError ParsedExpr'
readExpr' = readOrThrow parseExpr'

parseExpr' :: Parser ParsedExpr'
parseExpr' =
  (parseLiteral') <|>
  (startList *>
    (parseIf' <|>
     try parseAndOr' <|>
     parseLetOrLetStar' <|>
     parseFnApp' <|>
     parseDo')
    <* endList) <|>
  parseVarRef'

parseLiteral' :: Parser ParsedExpr'
parseLiteral' = attachPosition $ do
  lit <- (try (parseFixNum <|>
               parseStartingWithHash <|>
               parseNull))
  return $ literalP lit

parseFnApp' :: Parser ParsedExpr'
parseFnApp' = attachPosition $ do
  fnName <- parseVarName
  args <- try $ option [] $ many1 $ do
          atLeastOneSpace
          parseExpr'
  let constr = if isPrimitive fnName
               then primitiveAppP
               else userFnAppP
  return $ constr fnName args

attachPosition :: Parser (ExprPosition -> a) -> Parser a
attachPosition pf = do
  start <- readPosition
  f <- pf
  end <- readPosition
  return $ f (start, end)

parseIf' :: Parser ParsedExpr'
parseIf' = attachPosition $ do
  string "if"
  atLeastOneSpace
  cond <- parseExpr'
  atLeastOneSpace
  conseq <- parseExpr'
  atLeastOneSpace
  altern <- parseExpr'
  return $ _ifP cond conseq altern

parseListExpr' :: Parser [ParsedExpr']
parseListExpr' =
  parseExpr' `sepBy` atLeastOneSpace
  
parseAndOr' :: Parser ParsedExpr'
parseAndOr' = attachPosition $ do
  which <- (string "and") <|> (string "or")
  atLeastOneSpace
  args <- parseListExpr'
  spaces
  let constr = if which == "and" then _andP else _orP
  return $ constr args

parseBinding' :: Parser (BindingF ParsedExpr')
parseBinding' = surroundedByParensOrBrackets $ do
  varName <- parseVarName
  atLeastOneSpace
  expr <- parseExpr'
  spaces
  return $ (varName, expr)

parseLetOrLetStar' :: Parser ParsedExpr'
parseLetOrLetStar' = attachPosition $ do
  string "let"
  maybeStar <- optionMaybe (char '*')
  atLeastOneSpace
  bindings <- surroundedByParensOrBrackets $ parseBinding' `sepBy` atLeastOneSpace
  spaces
  body <- parseExpr'
  let constr = if maybeStar == Nothing then _letP else _letStarP
  return $ constr bindings body

parseVarRef' :: Parser ParsedExpr'
parseVarRef' =
  attachPosition $ varP <$> parseVarName

parseDo' :: Parser ParsedExpr'
parseDo' = attachPosition $ do
  string "do"
  atLeastOneSpace
  exprs <- parseListExpr'
  return $ _doP exprs

readPosition :: Parser Position
readPosition =
  toPosition <$> getPosition

toPosition :: SourcePos -> Position
toPosition sourcePos = Position (sourceLine sourcePos) (sourceColumn sourcePos)
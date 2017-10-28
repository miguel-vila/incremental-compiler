module Parser where

import Expr hiding (char)
import Primitives
import LiteralParser

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.HashMap hiding (map)
import Data.Char (isSpace)

data ParsingError = ParsingError { messages :: [String]
                                 , position :: String
                                 } deriving (Show, Eq)

toParsingError :: ParseError -> ParsingError
toParsingError parseError =
  ParsingError (map messageString (errorMessages parseError)) (show $ errorPos parseError)

readOrThrow :: Parser a -> String -> Either ParsingError a
readOrThrow parser input =
  either (Left . toParsingError) Right (parse parser "lisp" input)

readExpr :: String -> Either ParsingError ParsedExpr
readExpr = readOrThrow parseExpr

readProgram :: String -> Either ParsingError Program
readProgram = readOrThrow parseProgram

parseLambda :: Parser (LambdaF ParsedExpr)
parseLambda = do
  startList
  string "lambda"
  atLeastOneSpace
  startList
  args <- parseVarName `sepBy` atLeastOneSpace
  endList
  atLeastOneSpace
  body <- parseExpr
  endList
  return $ Lambda args body

parseLambdaBinding :: Parser (LambdaBindingF ParsedExpr)
parseLambdaBinding = do
  startList
  fnName <- parseVarName
  atLeastOneSpace
  lambda <- parseLambda
  endList
  return $ LambdaBinding fnName lambda

parseProgram :: Parser Program
parseProgram =
  (try parseLetRec) <|> (try $ Expr <$> parseExpr)

parseLetRec :: Parser Program
parseLetRec = do
  startList
  string "letrec"
  spaces
  startList
  lambdaBindings <- parseLambdaBinding `sepBy` atLeastOneSpace
  endList
  atLeastOneSpace
  body <- parseExpr
  endList
  return $ LetRec lambdaBindings body

parseExpr :: Parser ParsedExpr
parseExpr =
  parseLiteral <|>
  (startList *>
    (parseIf <|>
     try parseAndOr <|>
     parseLetOrLetStar <|>
     parseFnApp <|>
     parseDo)
    <* endList) <|>
  parseVarRef

parseLiteral :: Parser ParsedExpr
parseLiteral = literal <$> try (parseFixNum <|>
               parseStartingWithHash <|>
               parseNull)

isPrimitive :: FnName -> Bool
isPrimitive fnName = member fnName primitives

startList :: Parser Char
startList = char '('

endList :: Parser Char
endList = char ')'

atLeastOneSpace = many1 space

parseFnApp :: Parser ParsedExpr
parseFnApp = do
  fnName <- parseVarName
  args <- try $ option [] $ many1 $ do
          atLeastOneSpace
          parseExpr
  let constr = if isPrimitive fnName
               then primitiveApp
               else userFnApp
  return $ constr fnName args

parseIf :: Parser ParsedExpr
parseIf = do
  string "if"
  atLeastOneSpace
  cond <- parseExpr
  atLeastOneSpace
  conseq <- parseExpr
  atLeastOneSpace
  altern <- parseExpr
  return $ _if cond conseq altern

parseListExpr :: Parser [ParsedExpr]
parseListExpr =
  parseExpr `sepBy` atLeastOneSpace

reservedNames :: [String]
reservedNames = [ "or"
                , "and"
                , "do"
                ]

parseAndOr :: Parser ParsedExpr
parseAndOr = do
  which <- (string "and") <|> (string "or")
  atLeastOneSpace
  args <- parseListExpr
  spaces
  let constr = if which == "and" then _and else _or
  return $ constr args

parseVarName :: Parser VarName
parseVarName =
  let isValidChar x = not (isSpace x || x == '(' || x == ')' )
  in do mapM_ (notFollowedBy . string) reservedNames
        many1 (satisfy isValidChar)

parseBinding :: Parser (BindingF ParsedExpr)
parseBinding = surroundedByParensOrBrackets $ do
  varName <- parseVarName
  atLeastOneSpace
  expr <- parseExpr
  spaces
  return $ (varName, expr)

endFor :: Char -> Char
endFor '(' = ')'
endFor '[' = ']'

surroundedByParensOrBrackets :: Parser a -> Parser a
surroundedByParensOrBrackets p = do
  start <- (char '(') <|> (char '[')
  v <- p
  char $ endFor start
  return v

parseLetOrLetStar :: Parser ParsedExpr
parseLetOrLetStar = do
  string "let"
  maybeStar <- optionMaybe (char '*')
  atLeastOneSpace
  bindings <- surroundedByParensOrBrackets $ parseBinding `sepBy` atLeastOneSpace
  spaces
  body <- parseExpr
  let constr = if maybeStar == Nothing then _let else _letStar
  return $ constr bindings body

parseVarRef :: Parser ParsedExpr
parseVarRef = var <$> parseVarName

parseDo :: Parser ParsedExpr
parseDo = do
  string "do"
  atLeastOneSpace
  exprs <- parseListExpr
  return $ _do exprs

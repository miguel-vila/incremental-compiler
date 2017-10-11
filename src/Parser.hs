module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.HashMap hiding (map)
import Data.Char (isSpace)
import Control.Monad.State.Lazy
import Control.Monad.Except
import Numeric (readFloat)
import Expr hiding (char)
import Emitter

data ParsingError = ParsingError { messages :: [String]
                                 , position :: String
                                 } deriving (Show, Eq)

toParsingError :: ParseError -> ParsingError
toParsingError parseError =
  ParsingError (map messageString (errorMessages parseError)) (show $ errorPos parseError)

readOrThrow :: Parser a -> String -> Either ParsingError a
readOrThrow parser input =
  either (Left . toParsingError) Right (parse parser "lisp" input)

readExpr :: String -> Either ParsingError Expr
readExpr = readOrThrow parseExpr

readProgram :: String -> Either ParsingError Program
readProgram = readOrThrow parseProgram

parseLambda :: Parser Lambda
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

parseLambdaBinding :: Parser LambdaBinding
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

parseExpr :: Parser Expr
parseExpr =
  (L <$> try parseLiteral) <|>
  (startList *>
    (parseIf <|>
     try parseAndOr <|>
     parseLetOrLetStar <|>
     parseFnApp <|>
     parseDo)
    <* endList) <|>
  parseVarRef

parseLiteral :: Parser Literal
parseLiteral = parseFixNum <|>
               parseStartingWithHash <|>
               parseNull

parseFixNum :: Parser Literal
parseFixNum = do
  s <- getInput
  case readFloat s :: [(Float, String)] of
    [(n,s')] -> (FixNum $ floor n) <$ setInput s' -- @TODO how to avoid the `floor`?
    _ -> fail "Not an integer"

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

isPrimitive :: FnName -> Bool
isPrimitive fnName = member fnName primitives

startList :: Parser Char
startList = char '('

endList :: Parser Char
endList = char ')'

atLeastOneSpace = many1 space

parseFnApp :: Parser Expr
parseFnApp = do
  fnName <- parseVarName
  args <- try $ option [] $ many1 $ do
          atLeastOneSpace
          parseExpr
  let constr = if isPrimitive fnName
               then PrimitiveApp
               else UserFnApp
  return $ constr fnName args

parseIf :: Parser Expr
parseIf = do
  string "if"
  atLeastOneSpace
  cond <- parseExpr
  atLeastOneSpace
  conseq <- parseExpr
  atLeastOneSpace
  altern <- parseExpr
  return $ If cond conseq altern

parseListExpr :: Parser [Expr]
parseListExpr =
  parseExpr `sepBy` atLeastOneSpace

reservedNames :: [String]
reservedNames = [ "or"
                , "and"
                , "do"
                ]

parseAndOr :: Parser Expr
parseAndOr = do
  which <- (string "and") <|> (string "or")
  atLeastOneSpace
  args <- parseListExpr
  spaces
  let constr = if which == "and" then And else Or
  return $ constr args

parseVarName :: Parser VarName
parseVarName =
  let isValidChar x = not (isSpace x || x == '(' || x == ')' )
  in do mapM_ (notFollowedBy . string) reservedNames
        many1 (satisfy isValidChar)

parseBinding :: Parser Binding
parseBinding = surroundedByParensOrBrackets $ do
  varName <- parseVarName
  atLeastOneSpace
  expr <- parseExpr
  spaces
  return $ Binding varName expr

endFor :: Char -> Char
endFor '(' = ')'
endFor '[' = ']'

surroundedByParensOrBrackets :: Parser a -> Parser a
surroundedByParensOrBrackets p = do
  start <- (char '(') <|> (char '[')
  v <- p
  char $ endFor start
  return v

parseLetOrLetStar :: Parser Expr
parseLetOrLetStar = do
  string "let"
  maybeStar <- optionMaybe (char '*')
  atLeastOneSpace
  bindings <- surroundedByParensOrBrackets $ parseBinding `sepBy` atLeastOneSpace
  spaces
  body <- parseExpr
  let constr = if maybeStar == Nothing then Let else LetStar
  return $ constr bindings body

parseVarRef :: Parser Expr
parseVarRef = VarRef <$> parseVarName

parseDo :: Parser Expr
parseDo = do
  string "do"
  atLeastOneSpace
  exprs <- parseListExpr
  return $ Do exprs

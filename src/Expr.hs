module Expr where

type ParamName = String

type FunctionName = String

data Lambda = Lambda { params :: [ParamName]
                     , body   :: Expr
                     }

data Program = Expr Expr
             | LetRec [LambdaBinding] Expr

data Expr = L Literal
          | FnApp FunctionName [Expr]
          | If Expr Expr Expr
          | And [Expr]
          | Or [Expr]
          | Let     [Binding] Expr
          | LetStar [Binding] Expr
          | VarRef VarName
          | NoOp
          | UserFnApp FunctionName [Expr]
          deriving (Show, Eq)

data Literal = FixNum Integer
             | Boolean Bool
             | Character Char
             | Nil
             deriving (Show, Eq)

type VarName = String

data LambdaBinding = LambdaBinding { functionName :: String
                                   , lambda       :: Lambda
                                   }

data Binding = Binding { name :: VarName
                       , expr :: Expr
                       } deriving (Show, Eq)

_False :: Expr
_False = L $ Boolean False

_True :: Expr
_True = L$ Boolean True

fx :: Integer -> Expr
fx = L . FixNum

char :: Char -> Expr
char = L . Character

nil :: Expr
nil = L Nil

var :: String -> Expr
var = VarRef

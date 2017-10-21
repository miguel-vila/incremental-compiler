module CodeGen where

import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Data.HashMap hiding (map)
import Control.Monad.Reader
import Expr hiding (_and, _or)
import MagicNumbers

type Inst = String

type Code = [Inst]

type Op = String

type Register = String

-- The id for the function labels
type CodeGenState = Int

type IsTail = Bool

type StackIndex = Integer

data CompilationError = VariableNotInScope VarName
                      | FunctionNotDefined FnName
                      deriving (Show, Eq)

-- State CodeGenState (Reader (StackIndex, Environment, IsTail) (Either CompilationError (Writer Code)))
type GenReaderState = StateT CodeGenState (ReaderT (StackIndex, Environment, IsTail) (WriterT Code (Either CompilationError)))

type CodeGen = GenReaderState ()

type Label = String

wordsize :: Integer
wordsize = 4

data ComparisonType = Eq
                    | Less
                    | LessOrEq
                    | Greater
                    | GreaterOrEq
                    | NotEq

opposing :: ComparisonType -> ComparisonType
opposing Eq          = NotEq
opposing Less        = GreaterOrEq
opposing LessOrEq    = Greater
opposing Greater     = LessOrEq
opposing GreaterOrEq = Less
opposing NotEq       = Eq

comparisonToJump :: ComparisonType -> String
comparisonToJump Eq          = "je"
comparisonToJump Less        = "jl"
comparisonToJump LessOrEq    = "jle"
comparisonToJump Greater     = "jg"
comparisonToJump GreaterOrEq = "jge"
comparisonToJump NotEq       = "jne"

comparisonToSet :: ComparisonType -> String
comparisonToSet Eq          = "sete"
comparisonToSet Less        = "setl"
comparisonToSet LessOrEq    = "setle"
comparisonToSet Greater     = "setg"
comparisonToSet GreaterOrEq = "setge"
comparisonToSet NotEq       = "setne"

data FnGen = UnaryFn CodeGen
           | BinaryFn (Register -> Register -> CodeGen)
           | NaryFn (StackIndex -> CodeGen)
           | Predicate CodeGen
           | Comparison ComparisonType (Register -> CodeGen)

data Environment = Environment
  { varEnv :: Map VarName StackIndex
  , fnEnv  :: FnEnvironment
  }

type FnEnvironment = Map FnName Label

tabbed :: String -> String
tabbed s = "    " ++ s

emit :: String -> CodeGen
emit s = tell [ tabbed s ]

emitAll :: [String] -> CodeGen
emitAll = tell

emitNoTab :: String -> CodeGen
emitNoTab s = tell [s]

noop :: CodeGen
noop = tell []

getEnv :: GenReaderState Environment
getEnv = fmap (\(_,env,_) -> env) ask

getStackIndex :: GenReaderState StackIndex
getStackIndex = fmap (\(si,_,_) -> si) ask

isInTailPosition :: GenReaderState IsTail
isInTailPosition = fmap (\(_,_,isTail) -> isTail) ask

insertFnBinding :: FnName -> Label -> Environment -> Environment
insertFnBinding fnName label env =
  env { fnEnv = insert fnName label (fnEnv env) }

emitLiteral :: Integer -> CodeGen
emitLiteral n =
  movl ("$" ++ show n) "%eax"

applyMask :: Integer -> CodeGen
applyMask mask =
  _and ("$" ++ show mask) "%eax"

emitBooleanByComparison :: ComparisonType -> CodeGen
emitBooleanByComparison compareType = do
  emit $ comparisonToSet compareType ++ " %al"  -- set %al to the result of the comparison
  emit $ "movzbl %al, %eax"                   -- mov %al to %eax and pad the remaining bits with 0: https://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Move_with_zero_extend --> why is this needed?
  emit $ "sal $6, %al"                        -- move the result bit 6 bits to the left
  _or ("$" ++ show falseValue) "%al" -- or with the false value to return a "boolean" in the expected format

emitFunctionHeader :: Label -> CodeGen
emitFunctionHeader label = do
  emitNoTab $ ".globl " ++ label
  emitNoTab $ ".type " ++ label ++ ", @function"
  emitLabel label

withNextIndex :: GenReaderState a -> GenReaderState a
withNextIndex =
  local (\(si,env,isTail) -> (nextStackIndex si, env, isTail))

withEnv :: Environment -> GenReaderState a -> GenReaderState a
withEnv env =
  local (\(si,_,isTail) -> (si,env,isTail))

emitStackLoad :: StackIndex -> CodeGen
emitStackLoad si  = movl (stackValueAt si) "%eax"

emitStackSave :: CodeGen
emitStackSave = do
  sv <- stackValueAtIndex
  movl "%eax" sv

nextStackIndex :: StackIndex -> StackIndex
nextStackIndex si = si - wordsize

compareTo :: Integer -> CodeGen
compareTo n =
  cmp ("$" ++ show n) "%eax"

jumpTo :: Label -> CodeGen
jumpTo label =
  emit $ "jmp " ++ label

emitLabel :: Label -> CodeGen
emitLabel label =
  emitNoTab $ label ++ ":"

uniqueLabel :: GenReaderState Label
uniqueLabel = do
  s <- get
  modify (+1)
  return $ "L_" ++ show s

uniqueLambdaLabel :: String -> GenReaderState Label
uniqueLambdaLabel fnName = do
  s <- get
  modify (+1)
  return $ "Lambda_" ++ show s

stackValueAt :: StackIndex -> String
stackValueAt si = (show si) ++ "(%esp)"

stackValueAtIndex :: GenReaderState String
stackValueAtIndex = do
  si <- getStackIndex
  return $ stackValueAt si

binaryOp :: Op -> Register -> Register -> CodeGen
binaryOp op reg1 reg2 =
  emit $ op ++ " " ++ reg1 ++ ", " ++ reg2

addl :: Register -> Register -> CodeGen
addl = binaryOp "addl"

mov :: Register -> Register -> CodeGen
mov = binaryOp "mov"

movl :: Register -> Register -> CodeGen
movl = binaryOp "movl"

subl :: Register -> Register -> CodeGen
subl = binaryOp "subl"

shr :: Int -> Register -> CodeGen
shr n reg2 = binaryOp "shr" ("$" ++ show n) reg2

-- normally when we multiply two numbers they are shifted 2 bits
-- to the right. That's: a number x is represented by 4*x
-- So if we want to get the product of x and y we can produce
-- the number 4*x*y. This can be done by shifting one of the numbers
-- two bits to the left and then multiplying it by the other.
-- (4x) * y
-- 4(x*y)
fxTimes :: FnGen
fxTimes = BinaryFn $ \reg1 reg2 -> do
  shr intShift reg2
  emit $ "mull " ++ reg1

_and :: Register -> Register -> CodeGen
_and = binaryOp "and"

_or :: Register -> Register -> CodeGen
_or = binaryOp "or"

cmp :: Register -> Register -> CodeGen
cmp = binaryOp "cmp"

compareEaxToStackValue :: Register -> CodeGen
compareEaxToStackValue si = cmp "%eax" si

heapPosWithOffset :: Integer -> String
heapPosWithOffset offset =
  show offset ++ "(%ebp)"

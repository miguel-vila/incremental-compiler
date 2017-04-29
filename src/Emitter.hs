module Emitter where

import Control.Monad.Writer.Lazy
import MagicNumbers
import Expr
import InmediateRepr
import Control.Monad.State.Lazy

type Code = [String]

type CodeGenState = Int

type GenState = StateT CodeGenState (Writer Code)

type CodeGen = GenState ()

data FnGen = UnaryFn CodeGen
           | BinaryFn (Register -> Register -> CodeGen)
           | Predicate CodeGen
           | Comparison ComparisonType (Register -> CodeGen)

wordsize :: Integer
wordsize = 4

initialState :: CodeGenState
initialState = 0

compile :: Expr -> Code
compile = executeGen . wrapInEntryPoint . emitExpr (- wordsize)

executeGen :: CodeGen -> Code
executeGen codeGen = execWriter $ evalStateT codeGen initialState

tab :: String
tab = "    "

emit :: String -> CodeGen
emit s = tell [ tab ++ s]

emitNoTab :: String -> CodeGen
emitNoTab s = tell [s]

noop :: CodeGen
noop = tell []

emitFunctionHeader :: Label -> CodeGen
emitFunctionHeader label = do
  emit $ ".globl " ++ label
  emit $ ".type " ++ label ++ ", @function"
  emitLabel label

wrapInEntryPoint :: CodeGen -> CodeGen
wrapInEntryPoint code = do
  emit ".text"
  emitFunctionHeader "L_scheme_entry"
  code
  emit "ret"
  emitFunctionHeader "scheme_entry"
  emit "mov %esp, %ecx"      -- save C's stack pointer
  emit "mov 4(%esp), %esp"   -- load stack pointer from parameter
  emit "call L_scheme_entry"
  emit "mov %ecx, %esp"      -- restore C's stack pointer
  emit "ret"

emitLiteral :: Integer -> CodeGen
emitLiteral n = do
  emit $ "movl $" ++ show n ++ ", %eax"

applyMask :: Integer -> CodeGen
applyMask mask =
  emit $ "and $" ++ show mask ++ ", %al"

emitBooleanByComparison :: ComparisonType -> CodeGen
emitBooleanByComparison compareType = do
  emit $ comparisonToSet compareType ++ " %al"  -- set %al to the result of the comparison
  emit $ "movzbl %al, %eax"                   -- mov %al to %eax and pad the remaining bits with 0: https://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Move_with_zero_extend --> why is this needed?
  emit $ "sal $6, %al"                        -- move the result bit 6 bits to the left
  emit $ "or $" ++ show falseValue ++ ", %al" -- or with the false value to return a "boolean" in the expected format

ifEqReturnTrue :: CodeGen
ifEqReturnTrue = emitBooleanByComparison Eq

emitExpr :: StackIndex -> Expr -> CodeGen
emitExpr _ (L literal) =
  emitLiteral $ inmediateRepr literal
emitExpr si (FnApp name args) =
  let (Just primitive) = lookup name primitives  -- @TODO handle this
  in emitFnApp name si primitive args
emitExpr si (If condition conseq altern) =
  emitIf si condition conseq altern
emitExpr si (And preds) =
  emitAnd si preds
emitExpr si (Or preds) =
  emitOr si preds
emitExpr _ NoOp =
  noop

emitFnApp :: String -> StackIndex -> FnGen -> [Expr] -> CodeGen
emitFnApp fnName si fnGen args =
  case fnGen of
    UnaryFn codeGen -> do
      emitArgs si args -- @TODO check # of args
      codeGen
    BinaryFn codeGen ->
      if elem fnName canBeOptimized
      then emitBinaryFnOpt codeGen args si
      else emitBinaryFnApp codeGen args si
    Predicate predicate -> do
      emitArgs si args -- @TODO check # of args
      predicate
      ifEqReturnTrue
    Comparison compareType compareBody -> do
      emitArgs si args -- @TODO check # of args
      compareBody (stackValueAt si)
      emitBooleanByComparison compareType

canBeOptimized :: [String]
canBeOptimized = [ "fx+"
                 ]

emitBinaryFnApp :: (Register -> Register -> CodeGen) -> [Expr] -> StackIndex -> CodeGen
emitBinaryFnApp codeGen args si = do
    emitArgs si args
    codeGen (stackValueAt si) eax

emitBinaryFnOpt :: (Register -> Register -> CodeGen) -> [Expr] -> StackIndex -> CodeGen
emitBinaryFnOpt codeGen args si = case args of
  [L arg1, arg2] -> do
    emitExpr si arg2
    codeGen ("$" ++ (show $ inmediateRepr arg1)) eax
  [arg1, L arg2] -> do
    emitExpr si arg1
    codeGen ("$" ++ (show $ inmediateRepr arg2)) eax
  _              ->
    emitBinaryFnApp codeGen args si

emitArgs :: StackIndex -> [Expr] -> CodeGen
emitArgs si args = loop si args noop
  where loop si' [arg]           gen =
          do gen
             emitExpr si' arg
        loop si' (argh:argsTail) gen =
          let gen' = do gen
                        emitExpr si' argh
                        emit $ "movl %eax, " ++ stackValueAt si'
          in loop (si' - wordsize) argsTail gen'

primitives :: [(String, FnGen)]
primitives = unaryPrims ++ binaryPrims

unaryPrims :: [(String, FnGen)]
unaryPrims = [ ("fxadd1"      , fxadd1)
             , ("fxsub1"      , fxsub1)
             , ("char->fixnum", charToFixNum)
             , ("fixnum->char", fixNumToChar)
             , ("fxlognot"    , fxLogNot)
             , ("fixnum?"     , isFixnum)
             , ("null?"       , isNull)
             , ("not"         , notL)
             , ("boolean?"    , isBoolean)
             , ("char?"       , isChar)
             , ("fxzero?"     , isFxZero)
             ]

binaryPrims :: [(String, FnGen)]
binaryPrims = [ ("fx+"      , fxPlus)
              , ("fx-"      , fxMinus)
              , ("fx*"      , fxTimes)
              , ("fxlogand" , fxLogAnd)
              , ("fxlogor"  , fxLogOr)
              , ("fx="      , fxEq)
              , ("fx<"      , fxLess)
              , ("fx<="     , fxLessOrEq)
              , ("fx>"      , fxGreater)
              , ("fx>="     , fxGreaterOrEq)
              ]

fxadd1 :: FnGen
fxadd1 = UnaryFn $
  emit $ "addl $" ++ (show $ inmediateRepr $ FixNum 1) ++ ", %eax"

fxsub1 :: FnGen
fxsub1 = UnaryFn $
  emit $ "subl $" ++ (show $ inmediateRepr $ FixNum 1) ++ ", %eax"

charToFixNum :: FnGen
charToFixNum = UnaryFn $
  emit $ "sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: FnGen
fixNumToChar = UnaryFn $ do
  emit $ "sall $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the left 6 bits
  emit $ "orl $" ++ show charTag ++ ", %eax" -- add char tag

compareTo :: Integer -> CodeGen
compareTo n =
  emit $ "cmp $" ++ show n ++ ", %al"

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

type Label = String

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

ifComparisonJumpTo :: ComparisonType -> Label -> CodeGen
ifComparisonJumpTo compareType label =
  emit $ comparisonToJump compareType ++" " ++ label

ifEqJumpTo :: Label -> CodeGen
ifEqJumpTo = ifComparisonJumpTo Eq

ifNotEqJumpTo :: Label -> CodeGen
ifNotEqJumpTo = ifComparisonJumpTo NotEq

jumpTo :: Label -> CodeGen
jumpTo label =
  emit $ "jmp " ++ label

emitLabel :: Label -> CodeGen
emitLabel label =
  emitNoTab $ label ++ ":"

isNull :: FnGen
isNull = Predicate $ compareTo nilValue

isBoolean :: FnGen
isBoolean = Predicate $ do
  applyMask boolMask
  compareTo falseValue

isChar :: FnGen
isChar = Predicate $ do
    applyMask charMask
    compareTo $ toInteger charTag

isFixnum :: FnGen
isFixnum = Predicate $ do
  applyMask $ toInteger intTag
  compareTo 0

notL :: FnGen
notL = Predicate $ compareTo falseValue

isFxZero :: FnGen
isFxZero = Predicate $ compareTo 0

fxLogNot :: FnGen
fxLogNot = UnaryFn $ do
  emit "not %eax"
  emit "sar $2, %eax"
  emit "sal $2, %eax"

uniqueLabel :: GenState Label
uniqueLabel = do
  s <- get
  modify (+1)
  return $ "L_" ++ show s

emitIf :: StackIndex -> Expr -> Expr -> Expr -> CodeGen
emitIf si condition conseq altern = do
  alternLabel <- uniqueLabel
  endLabel    <- uniqueLabel
  let emitIfFor :: CodeGen -> CodeGen
      emitIfFor code = do
        code
        compareTo falseValue
        ifEqJumpTo alternLabel
  let evalCondAndJumpToAlternIfFalse =
        case condition of
          FnApp fnName args ->
            let (Just primitive) = lookup fnName primitives  -- @TODO handle this
                emitAndJump (UnaryFn fnCode) =
                  emitIfFor fnCode
                emitAndJump (BinaryFn fnCode) =
                  emitIfFor $ fnCode (stackValueAt si) eax
                emitAndJump (Predicate predicateCode) = do
                  predicateCode
                  ifNotEqJumpTo alternLabel
                emitAndJump (Comparison comparisonType fnCode)  = do
                  fnCode (stackValueAt si)
                  ifComparisonJumpTo (opposing comparisonType) alternLabel
            in do emitArgs si args
                  emitAndJump primitive
          _ -> do emitExpr si condition
                  compareTo falseValue
                  ifEqJumpTo alternLabel
  evalCondAndJumpToAlternIfFalse
  emitExpr si conseq
  jumpTo endLabel
  emitLabel alternLabel
  emitExpr si altern
  emitLabel endLabel

emitAnd :: StackIndex -> [Expr] -> CodeGen
emitAnd si []            = emitExpr si _False
emitAnd si [test]        = emitExpr si test
emitAnd si (test : rest) = emitIf si test (And rest) _False

emitOr :: StackIndex -> [Expr] -> CodeGen
emitOr si []            = emitExpr si _True
emitOr si [test]        = emitExpr si test
emitOr si (test : rest) = emitIf si test NoOp (Or rest)

type StackIndex = Integer

stackValueAt :: StackIndex -> String
stackValueAt si = show si ++ "(%esp)"

type Inst = String
type Op = String
type Register = String

binOp :: Op -> Register -> Register -> CodeGen
binOp op reg1 reg2 =
  emit $ op ++ " " ++ reg1 ++ ", " ++ reg2

eax :: String
eax = "%eax"

addl :: Register -> Register -> CodeGen
addl = binOp "addl"

fxPlus :: FnGen
fxPlus = BinaryFn addl

mov :: Register -> Register -> CodeGen
mov reg1 reg2 = binOp "mov" reg1 reg2

subl :: Register -> Register -> CodeGen
subl = binOp "subl"

fxMinus :: FnGen
fxMinus = BinaryFn $ \reg1 reg2 -> do
  subl reg2 reg1
  mov  reg1 reg2

shr :: Int -> Register -> CodeGen
shr n reg2 = binOp "shr" ("$" ++ show n) reg2

-- normally when we multiply two numbers they are shifted 2 bits
-- to the right. That's a number x is represented by 4*x
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
_and = binOp "and"

fxLogAnd :: FnGen
fxLogAnd = BinaryFn _and

_or :: Register -> Register -> CodeGen
_or = binOp "or"

fxLogOr :: FnGen
fxLogOr = BinaryFn _or

cmp :: Register -> Register -> CodeGen
cmp = binOp "cmp"

compareEaxToStackValue :: Register -> CodeGen
compareEaxToStackValue si = cmp "%eax" si

fxComparison :: ComparisonType -> FnGen
fxComparison comparisonType =
  Comparison comparisonType compareEaxToStackValue

fxEq :: FnGen
fxEq = fxComparison Eq

fxLess :: FnGen
fxLess = fxComparison Less

fxLessOrEq :: FnGen
fxLessOrEq = fxComparison LessOrEq

fxGreater :: FnGen
fxGreater = fxComparison Greater

fxGreaterOrEq :: FnGen
fxGreaterOrEq = fxComparison GreaterOrEq

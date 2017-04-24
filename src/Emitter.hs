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
emitBooleanByComparison cmp = do
  emit $ comparisonToSet cmp ++ " %al"  -- set %al to the result of equals
  emit $ "movzbl %al, %eax"                   -- mov %al to %eax and pad the remaining bits with 0: https://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Move_with_zero_extend --> why is this needed?
  emit $ "sal $6, %al"                        -- move the result bit 6 bits to the left
  emit $ "or $" ++ show falseValue ++ ", %al" -- or with the false value to return a "boolean" in the expected format

defaultPredicateCont :: CodeGen
defaultPredicateCont = emitBooleanByComparison Eq

emitExpr :: StackIndex -> Expr -> CodeGen
emitExpr _ (FixNum n) =
  emitLiteral $ inmediateRepr n
emitExpr _ (Boolean bool) =
  emitLiteral $ inmediateRepr bool
emitExpr _ (Character c) =
  emitLiteral $ inmediateRepr c
emitExpr _ Nil =
  emitLiteral nilValue
emitExpr si (UnaryFnApp name arg) =
  let (Just unaryPrim) = lookup name unaryPrims  -- @TODO handle this
  in emitUnaryFn si unaryPrim arg
emitExpr si (If condition conseq altern) =
  emitIf si condition conseq altern
emitExpr si (And preds) =
  emitAnd si preds
emitExpr si (Or preds) =
  emitOr si preds
emitExpr _ NoOp =
  noop
emitExpr si (BinaryFnApp name arg1 arg2) =
  let (Just binaryPrim) = lookup name binaryPrims -- @TODO handle this
  in emitBinaryFn si arg1 arg2 binaryPrim

emitUnaryFn :: StackIndex -> UnaryFunGen -> Expr -> CodeGen
emitUnaryFn si (ReturnValueFn body) arg = do
  emitExpr si arg
  body
emitUnaryFn si (Predicate predicate) arg = do
  emitExpr si arg
  predicate
  defaultPredicateCont

emitTwoArgs :: StackIndex -> Expr -> Expr -> CodeGen
emitTwoArgs si arg1 arg2 = do
  emitExpr si arg1
  emit $ "movl %eax, " ++ show si ++ "(%esp)"
  emitExpr (si - wordsize) arg2

emitBinaryFn :: StackIndex -> Expr -> Expr -> BinaryFnGen -> CodeGen
emitBinaryFn si arg1 arg2 binaryPrim = do
  emitTwoArgs si arg1 arg2
  case binaryPrim of
    ReturnValueBiFn fn -> fn si
    Comparison compareType compareBody -> do
      compareBody si
      emitBooleanByComparison compareType

data UnaryFunGen = ReturnValueFn CodeGen
                 | Predicate CodeGen

unaryPrims :: [(String, UnaryFunGen)]
unaryPrims = [ ("fxadd1"      , fxadd1)
             , ("fxsub1"      , fxsub1)
             , ("char->fixnum", charToFixNum)
             , ("fixnum->char", fixNumToChar)
             , ("fxlognot"    , fxLognot)
             , ("fixnum?"     , isFixnum)
             , ("null?"       , isNull)
             , ("not"         , notL)
             , ("boolean?"    , isBoolean)
             , ("char?"       , isChar)
             , ("fxzero?"     , isFxZero)
             ]

data BinaryFnGen = ReturnValueBiFn (StackIndex -> CodeGen)
                 | Comparison ComparisonType (StackIndex -> CodeGen)

binaryPrims :: [(String, BinaryFnGen)]
binaryPrims = [ ("fx+", fxPlus)
              , ("fx-", fxMinus)
              , ("fxlogand", fxLogAnd)
              , ("fxlogor", fxLogOr)
              , ("fx=", fxEq)
              , ("fx<", fxLess)
              , ("fx<=", fxLessOrEq)
              , ("fx>", fxGreater)
              , ("fx>=", fxGreaterOrEq)
              ]

fxadd1 :: UnaryFunGen
fxadd1 = ReturnValueFn $
  emit $ "addl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

fxsub1 :: UnaryFunGen
fxsub1 = ReturnValueFn $
  emit $ "subl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

charToFixNum :: UnaryFunGen
charToFixNum = ReturnValueFn $
  emit $ "sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: UnaryFunGen
fixNumToChar = ReturnValueFn $ do
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
ifComparisonJumpTo cmp label =
  emit $ comparisonToJump cmp ++" " ++ label

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

isNull :: UnaryFunGen
isNull = Predicate $ compareTo nilValue

isBoolean :: UnaryFunGen
isBoolean = Predicate $ do
  applyMask boolMask
  compareTo falseValue

isChar :: UnaryFunGen
isChar = Predicate $ do
    applyMask charMask
    compareTo $ toInteger charTag

isFixnum :: UnaryFunGen
isFixnum = Predicate $ do
  applyMask $ toInteger intTag
  compareTo 0

notL :: UnaryFunGen
notL = Predicate $ compareTo falseValue

isFxZero :: UnaryFunGen
isFxZero = Predicate $ compareTo 0

fxLognot :: UnaryFunGen
fxLognot = ReturnValueFn $ do
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
          UnaryFnApp fnName arg ->
            let (Just unaryPrim) = lookup fnName unaryPrims  -- @TODO handle this
                emitAndJump (ReturnValueFn fnCode)    = emitIfFor fnCode
                emitAndJump (Predicate predicateCode) = do
                  predicateCode
                  ifNotEqJumpTo alternLabel
            in do emitExpr si arg
                  emitAndJump unaryPrim
          BinaryFnApp fnName arg1 arg2 ->
            let (Just binaryPrim) = lookup fnName binaryPrims -- @TODO handle this
                emitAndJump (ReturnValueBiFn fnCode) = emitIfFor (fnCode si)
                emitAndJump (Comparison cmp fnCode)  = do
                  fnCode si
                  ifComparisonJumpTo (opposing cmp) alternLabel
            in do emitTwoArgs si arg1 arg2
                  emitAndJump binaryPrim
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
emitAnd si []            = emitExpr si (Boolean False)
emitAnd si [test]        = emitExpr si test
emitAnd si (test : rest) = emitIf si test (And rest) (Boolean False)

emitOr :: StackIndex -> [Expr] -> CodeGen
emitOr si []            = emitExpr si (Boolean True)
emitOr si [test]        = emitExpr si test
emitOr si (test : rest) = emitIf si test NoOp (Or rest)

type StackIndex = Integer

stackValueAt :: StackIndex -> String
stackValueAt si = show si ++ "(%esp)"

fxPlus :: BinaryFnGen
fxPlus = ReturnValueBiFn $ \si ->
  emit $ "addl " ++ stackValueAt si ++ ", %eax"

emitStackLoad :: StackIndex -> CodeGen
emitStackLoad si =
  emit $ "mov " ++ stackValueAt si ++ ", %eax"

fxMinus :: BinaryFnGen
fxMinus = ReturnValueBiFn $ \si -> do
  emit $ "subl %eax, " ++ stackValueAt si
  emitStackLoad si

fxLogAnd :: BinaryFnGen
fxLogAnd = ReturnValueBiFn $ \si ->
  emit $ "and " ++ stackValueAt si ++ ", %eax"

fxLogOr :: BinaryFnGen
fxLogOr = ReturnValueBiFn $ \si ->
  emit $ "or " ++ stackValueAt si ++ ", %eax"

compareEaxToStackValue :: StackIndex -> CodeGen
compareEaxToStackValue si =
  emit $ "cmp %eax, " ++ stackValueAt si

fxComparison :: ComparisonType -> BinaryFnGen
fxComparison cmp = Comparison cmp compareEaxToStackValue

fxEq :: BinaryFnGen
fxEq = fxComparison Eq

fxLess :: BinaryFnGen
fxLess = fxComparison Less

fxLessOrEq :: BinaryFnGen
fxLessOrEq = fxComparison LessOrEq

fxGreater :: BinaryFnGen
fxGreater = fxComparison Greater

fxGreaterOrEq :: BinaryFnGen
fxGreaterOrEq = fxComparison GreaterOrEq

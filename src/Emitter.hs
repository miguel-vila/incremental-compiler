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
  emit $ comparisonToSet cmp ++ " %al"  -- set %al to the result of the comparison
  emit $ "movzbl %al, %eax"                   -- mov %al to %eax and pad the remaining bits with 0: https://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Move_with_zero_extend --> why is this needed?
  emit $ "sal $6, %al"                        -- move the result bit 6 bits to the left
  emit $ "or $" ++ show falseValue ++ ", %al" -- or with the false value to return a "boolean" in the expected format

ifEqReturnTrue :: CodeGen
ifEqReturnTrue = emitBooleanByComparison Eq

emitExpr :: StackIndex -> Expr -> CodeGen
emitExpr _ (FixNum n) =
  emitLiteral $ inmediateRepr n
emitExpr _ (Boolean bool) =
  emitLiteral $ inmediateRepr bool
emitExpr _ (Character c) =
  emitLiteral $ inmediateRepr c
emitExpr _ Nil =
  emitLiteral nilValue
emitExpr si (FnApp name args) =
  let (Just primitive) = lookup name primitives  -- @TODO handle this
  in emitFnApp si primitive args
emitExpr si (If condition conseq altern) =
  emitIf si condition conseq altern
emitExpr si (And preds) =
  emitAnd si preds
emitExpr si (Or preds) =
  emitOr si preds
emitExpr _ NoOp =
  noop

emitFnApp :: StackIndex -> FnGen -> [Expr] -> CodeGen
emitFnApp si fnGen args = do
  emitArgs si args -- @TODO check # of args
  case fnGen of
    SimpleFn codeGen ->
      codeGen si
    Predicate predicate -> do
      predicate
      ifEqReturnTrue
    Comparison compareType compareBody -> do
      compareBody si
      emitBooleanByComparison compareType

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

data FnGen = SimpleFn (StackIndex -> CodeGen)
           | Predicate CodeGen
           | Comparison ComparisonType (StackIndex -> CodeGen)

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
fxadd1 = SimpleFn $ const $
  emit $ "addl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

fxsub1 :: FnGen
fxsub1 = SimpleFn $ const $
  emit $ "subl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

charToFixNum :: FnGen
charToFixNum = SimpleFn $ const $
  emit $ "sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: FnGen
fixNumToChar = SimpleFn $ const $ do
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
fxLogNot = SimpleFn $ const $ do
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
                emitAndJump (SimpleFn fnCode)    = emitIfFor $ fnCode si
                emitAndJump (Predicate predicateCode) = do
                  predicateCode
                  ifNotEqJumpTo alternLabel
                emitAndJump (Comparison cmp fnCode)  = do
                  fnCode si
                  ifComparisonJumpTo (opposing cmp) alternLabel
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

fxPlus :: FnGen
fxPlus = SimpleFn $ \si ->
  emit $ "addl " ++ stackValueAt si ++ ", %eax"

emitStackLoad :: StackIndex -> CodeGen
emitStackLoad si =
  emit $ "mov " ++ stackValueAt si ++ ", %eax"

fxMinus :: FnGen
fxMinus = SimpleFn $ \si -> do
  emit $ "subl %eax, " ++ stackValueAt si
  emitStackLoad si

-- normally when we multiply two numbers they are shifted 2 bits
-- to the right. That's a number x is represented by 4*x
-- So if we want to get the product of x and y we can produce
-- the number 4*x*y. This can be done by shifting one of the numbers
-- two bits to the left and then multiplying it by the other.
-- (4x) * y
-- 4(x*y)
fxTimes :: FnGen
fxTimes = SimpleFn $ \si -> do
  emit $ "shr $" ++ show intShift ++ ", %eax"
  emit $ "mull " ++ stackValueAt si

fxLogAnd :: FnGen
fxLogAnd = SimpleFn $ \si ->
  emit $ "and " ++ stackValueAt si ++ ", %eax"

fxLogOr :: FnGen
fxLogOr = SimpleFn $ \si ->
  emit $ "or " ++ stackValueAt si ++ ", %eax"

compareEaxToStackValue :: StackIndex -> CodeGen
compareEaxToStackValue si =
  emit $ "cmp %eax, " ++ stackValueAt si

fxComparison :: ComparisonType -> FnGen
fxComparison cmp = Comparison cmp compareEaxToStackValue

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

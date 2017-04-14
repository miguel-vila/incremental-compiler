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

initialState :: CodeGenState
initialState = 0

compile :: Expr -> Code
compile = executeGen . wrapInEntryPoint . emitExpr

executeGen :: CodeGen -> Code
executeGen codeGen = execWriter $ evalStateT codeGen initialState

emit :: String -> CodeGen
emit s = tell [s]

noop :: CodeGen
noop = tell []

wrapInEntryPoint :: CodeGen -> CodeGen
wrapInEntryPoint code = do
  emit "    .text"
  emit "    .globl scheme_entry"
  emit "    .type scheme_entry, @function"
  emit "scheme_entry:"
  code
  emit "    ret"

emitLiteral :: Integer -> CodeGen
emitLiteral n = do
  emit $ "    movl $" ++ show n ++ ", %eax"

applyMask :: Integer -> CodeGen
applyMask mask =
  emit $ "    and $" ++ show mask ++ ", %al"

defaultPredicateCont :: CodeGen
defaultPredicateCont = do
  emit $ "    sete %al"                           -- set %al to the result of equals
  emit $ "    movzbl %al, %eax"                   -- mov %al to %eax and pad the remaining bits with 0: https://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Move_with_zero_extend --> why is this needed?
  emit $ "    sal $6, %al"                        -- move the result bit 6 bits to the left
  emit $ "    or $" ++ show falseValue ++ ", %al" -- or with the false value to return a "boolean" in the expected format

emitExpr :: Expr -> CodeGen
emitExpr (FixNum n) =
  emitLiteral $ inmediateRepr n
emitExpr (Boolean bool) =
  emitLiteral $ inmediateRepr bool
emitExpr (Character c) =
  emitLiteral $ inmediateRepr c
emitExpr Nil =
  emitLiteral nilValue
emitExpr (UnaryFnApp name arg) =
  let (Just unaryPrim) = lookup name unaryPrims  -- @TODO handle this
  in emitUnaryFn unaryPrim arg
emitExpr (If condition conseq altern) =
  emitIf condition conseq altern
emitExpr (And preds) =
  emitAnd preds
emitExpr (Or preds) =
  emitOr preds
emitExpr NoOp =
  noop

emitUnaryFn :: UnaryFunGen -> Expr -> CodeGen
emitUnaryFn (ReturnValueFn body) arg = do
  emitExpr arg
  body
emitUnaryFn (Predicate predicate) arg = do
  emitExpr arg
  predicate
  defaultPredicateCont

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

fxadd1 :: UnaryFunGen
fxadd1 = ReturnValueFn $
  emit $ "    addl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

fxsub1 :: UnaryFunGen
fxsub1 = ReturnValueFn $
  emit $ "    subl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

charToFixNum :: UnaryFunGen
charToFixNum = ReturnValueFn $
  emit $ "    sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: UnaryFunGen
fixNumToChar = ReturnValueFn $ do
  emit $ "    sall $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the left 6 bits
  emit $ "    orl $" ++ show charTag ++ ", %eax" -- add char tag

compareTo :: Integer -> CodeGen
compareTo n =
  emit $ "    cmp $" ++ show n ++ ", %al"

data Comparison = Eq
                | NotEq

type Label = String

comparisonToJump :: Comparison -> String
comparisonToJump Eq    = "je"
comparisonToJump NotEq = "jne"

ifComparisonJumpTo :: Comparison -> Label -> CodeGen
ifComparisonJumpTo cmp label =
  emit $ "    " ++ comparisonToJump cmp ++" " ++ label

ifEqJumpTo :: Label -> CodeGen
ifEqJumpTo = ifComparisonJumpTo Eq

ifNotEqJumpTo :: Label -> CodeGen
ifNotEqJumpTo = ifComparisonJumpTo NotEq

jumpTo :: Label -> CodeGen
jumpTo label =
  emit $ "    jmp " ++ label

labelStart :: Label -> CodeGen
labelStart label =
  emit $ "    " ++ label ++ ":"

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
  emit "    not %eax"
  emit "    sar $2, %eax"
  emit "    sal $2, %eax"

uniqueLabel :: GenState Label
uniqueLabel = do
  s <- get
  modify (+1)
  return $ "L_" ++ show s

emitIf :: Expr -> Expr -> Expr -> CodeGen
emitIf condition conseq altern = do
  alternLabel <- uniqueLabel
  endLabel    <- uniqueLabel
  let evalCondAndJumpToAlternIfFalse =
        case condition of
          UnaryFnApp fnName arg ->
            let (Just unaryPrim) = lookup fnName unaryPrims  -- @TODO handle this
                evalAndJump (ReturnValueFn fnCode) = do
                  fnCode
                  compareTo falseValue
                  ifEqJumpTo alternLabel
                evalAndJump (Predicate predicateCode) = do
                  predicateCode
                  ifNotEqJumpTo alternLabel
            in do emitExpr arg
                  evalAndJump unaryPrim
          _ -> do emitExpr condition
                  compareTo falseValue
                  ifEqJumpTo alternLabel
  evalCondAndJumpToAlternIfFalse
  emitExpr conseq
  jumpTo endLabel
  labelStart alternLabel
  emitExpr altern
  labelStart endLabel

emitAnd :: [Expr] -> CodeGen
emitAnd []            = emitExpr (Boolean False)
emitAnd [test]        = emitExpr test
emitAnd (test : rest) = emitIf test (And rest) (Boolean False)

emitOr :: [Expr] -> CodeGen
emitOr []            = emitExpr (Boolean True)
emitOr [test]        = emitExpr test
emitOr (test : rest) = emitIf test NoOp (Or rest)

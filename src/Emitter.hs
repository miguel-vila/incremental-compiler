module Emitter where

import Control.Monad.Writer.Lazy
import MagicNumbers
import Expr
import InmediateRepr
import Control.Monad.State.Strict

type Code = [String]

type CodeGenState = Int

type GenState = StateT CodeGenState (Writer Code)

type CodeGen = GenState ()

initialState :: CodeGenState
initialState = 0

compile :: Expr -> Code
compile code = executeGen (wrapInFn $ emitExpr code)

executeGen :: CodeGen -> Code
executeGen codeGen = execWriter $ evalStateT codeGen initialState

emit :: String -> CodeGen
emit s = tell [s]

noop :: CodeGen
noop = tell []

wrapInFn :: CodeGen -> CodeGen
wrapInFn code = do
  emit "    .text"
  emit "    .globl scheme_entry"
  emit "    .type scheme_entry, @function"
  emit "scheme_entry:"
  code
  emit "    ret"

emitLiteral :: Integer -> CodeGen
emitLiteral n = do
    emit $ "    movl $" ++ (show n) ++ ", %eax"

applyMask :: Integer -> CodeGen
applyMask mask =
  emit $ "    and $" ++ show mask ++ ", %al"

returnTrueIfEqualTo :: Integer -> CodeGen
returnTrueIfEqualTo n = do
  emit $ "    cmp $" ++ show n ++ ", %al"         -- compare with n
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
  in do emitExpr arg
        unaryPrim
emitExpr (If condition conseq altern) =
  emitIf condition conseq altern
emitExpr (And preds) =
  emitAnd preds
emitExpr (Or preds) =
  emitOr preds
emitExpr NoOp =
  noop

unaryPrims :: [(String, CodeGen)]
unaryPrims = [ ("fxadd1", fxadd1)
             , ("fxsub1", fxsub1)
             , ("char->fixnum", charToFixNum)
             , ("fixnum->char", fixNumToChar)
             , ("fixnum?", isFixnum)
             , ("null?", isNull)
             , ("not", notL)
             , ("boolean?", isBoolean)
             , ("char?", isChar)
             , ("fxzero?", isFxZero)
             , ("fxlognot", fxLognot)
             ]

fxadd1 :: CodeGen
fxadd1 =
  emit $ "    addl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

fxsub1 :: CodeGen
fxsub1 =
  emit $ "    subl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

charToFixNum :: CodeGen
charToFixNum =
  emit $ "    sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: CodeGen
fixNumToChar = do
  emit $ "    sall $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the left 6 bits
  emit $ "    orl $" ++ show charTag ++ ", %eax" -- add char tag

isNull :: CodeGen
isNull = returnTrueIfEqualTo nilValue

isBoolean :: CodeGen
isBoolean = do
  applyMask boolMask
  returnTrueIfEqualTo falseValue

isChar :: CodeGen
isChar = do
    applyMask charMask
    returnTrueIfEqualTo $ toInteger charTag

isFixnum :: CodeGen
isFixnum = do
  applyMask $ toInteger intTag
  returnTrueIfEqualTo 0

notL :: CodeGen
notL = returnTrueIfEqualTo falseValue

isFxZero :: CodeGen
isFxZero = returnTrueIfEqualTo 0

fxLognot :: CodeGen
fxLognot = do
  emit "    not %eax"
  emit "    sar $2, %eax"
  emit "    sal $2, %eax"

uniqueLabel :: GenState String
uniqueLabel = do
  s <- get
  modify (+1)
  return $ "L_" ++ show s

emitIf :: Expr -> Expr -> Expr -> CodeGen
emitIf condition conseq altern = do
  alternLabel <- uniqueLabel
  endLabel    <- uniqueLabel
  emitExpr condition
  emit $ "    cmp $" ++ show falseValue ++ ", %al"
  emit $ "    je " ++ alternLabel
  emitExpr conseq
  emit $ "    jmp " ++ endLabel
  emit $ alternLabel ++ ":"
  emitExpr altern
  emit $ endLabel ++ ":"

emitAnd :: [Expr] -> CodeGen
emitAnd []            = emitExpr (Boolean False)
emitAnd [test]        = emitExpr test
emitAnd (test : rest) = emitIf test (And rest) (Boolean False)

emitOr :: [Expr] -> CodeGen
emitOr []            = emitExpr (Boolean True)
emitOr [test]        = emitExpr test
emitOr (test : rest) = emitIf test NoOp (Or rest)

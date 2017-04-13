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
  in unaryPrim arg
emitExpr (If condition conseq altern) =
  emitIf condition conseq altern

type UnaryPrim = (String, Expr -> CodeGen)

unaryPrims :: [UnaryPrim]
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

unaryPrim :: CodeGen -> Expr -> CodeGen
unaryPrim prim arg = do
  emitExpr arg
  prim

fxadd1 :: Expr -> CodeGen
fxadd1 = unaryPrim $
  emit $ "    addl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

fxsub1 :: Expr -> CodeGen
fxsub1 = unaryPrim $
  emit $ "    subl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

charToFixNum :: Expr -> CodeGen
charToFixNum = unaryPrim $
  emit $ "    sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: Expr -> CodeGen
fixNumToChar = unaryPrim $ do
  emit $ "    sall $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the left 6 bits
  emit $ "    orl $" ++ show charTag ++ ", %eax" -- add char tag

isNull :: Expr -> CodeGen
isNull = unaryPrim $ returnTrueIfEqualTo nilValue

isBoolean :: Expr -> CodeGen
isBoolean = unaryPrim $ do
  applyMask boolMask
  returnTrueIfEqualTo falseValue

isChar :: Expr -> CodeGen
isChar = unaryPrim $ do
    applyMask charMask
    returnTrueIfEqualTo $ toInteger charTag

isFixnum :: Expr -> CodeGen
isFixnum = unaryPrim $ do
  applyMask $ toInteger intTag
  returnTrueIfEqualTo 0

notL :: Expr -> CodeGen
notL = unaryPrim $ returnTrueIfEqualTo falseValue

isFxZero :: Expr -> CodeGen
isFxZero = unaryPrim $ returnTrueIfEqualTo 0

fxLognot :: Expr -> CodeGen
fxLognot = unaryPrim $ do
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

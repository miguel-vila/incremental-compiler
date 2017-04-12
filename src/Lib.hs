module Lib
    ( compileAndExecute
    ) where

import Control.Monad.Writer.Lazy
import System.Process
import System.IO
import System.Directory
import Parser
import Expr
import InmediateRepr
import MagicNumbers

compileAndExecute :: Expr -> IO String
compileAndExecute source = do
  let program = emitProgram source
  let programText = toText program
  writeFile "tmp-program.s" programText
  let command = "gcc -o tmp-program runtime.c tmp-program.s"
  callCommand command
  (_,handle,_,processHandle) <- runInteractiveCommand "./tmp-program"
  exitCode <- waitForProcess processHandle
  output <- hGetLine handle
  mapM_ removeFile ["tmp-program.s", "tmp-program"]
  return output

type Code = Writer [String] ()

emit :: String -> Code
emit s = tell [s]

emitProgram :: Expr -> Code
emitProgram = wrapInFn . emitExpr

wrapInFn :: Code -> Code
wrapInFn code = do
  emit "    .text"
  emit "    .globl scheme_entry"
  emit "    .type scheme_entry, @function"
  emit "scheme_entry:"
  code
  emit "    ret"

emitExpr :: Expr -> Code
emitExpr (FixNum n) =
  emitLiteral $ inmediateRepr n
emitExpr (Boolean bool) =
  emitLiteral $ inmediateRepr bool
emitExpr (Character c) =
  emitLiteral $ inmediateRepr c
emitExpr Nil =
  emitLiteral nilValue
emitExpr (UnaryFnApp name arg) =
  let (Just unaryPrim) = lookup name unaryPrims
  in unaryPrim arg

emitLiteral :: Integer -> Code
emitLiteral n = do
    emit ("    movl $" ++ (show n) ++ ", %eax")

toText :: Code -> String
toText =
  unlines . snd . runWriter

unaryPrim :: Code -> Expr -> Code
unaryPrim prim arg = do
  emitExpr arg
  prim

fxadd1 :: Expr -> Code
fxadd1 = unaryPrim $
  emit $ "    addl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

fxsub1 :: Expr -> Code
fxsub1 = unaryPrim $
  emit $ "    subl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

charToFixNum :: Expr -> Code
charToFixNum = unaryPrim $
  emit $ "    sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: Expr -> Code
fixNumToChar = unaryPrim $ do
  emit $ "    sall $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the left 6 bits
  emit $ "    orl $" ++ show charTag ++ ", %eax" -- add char tag

isNull :: Expr -> Code
isNull = unaryPrim $ returnTrueIfEqualTo nilValue

applyMask :: Integer -> Code
applyMask mask =
  emit $ "    and $" ++ show mask ++ ", %al"

isBoolean :: Expr -> Code
isBoolean = unaryPrim $ do
  applyMask boolMask
  returnTrueIfEqualTo falseValue

isChar :: Expr -> Code
isChar = unaryPrim $ do
    applyMask charMask
    returnTrueIfEqualTo $ toInteger charTag

isFixnum :: Expr -> Code
isFixnum = unaryPrim $ do
  applyMask $ toInteger intTag
  returnTrueIfEqualTo 0

notL :: Expr -> Code
notL = unaryPrim $ returnTrueIfEqualTo falseValue

isFxZero :: Expr -> Code
isFxZero = unaryPrim $ returnTrueIfEqualTo 0

returnTrueIfEqualTo :: Integer -> Code
returnTrueIfEqualTo n = do
  emit $ "    cmp $" ++ show n ++ ", %al"         -- compare with n
  emit $ "    sete %al"                           -- set %al to the result of equals
  emit $ "    movzbl %al, %eax"                   -- mov %al to %eax and pad the remaining bits with 0: https://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Move_with_zero_extend --> why is this needed?
  emit $ "    sal $6, %al"                        -- move the result bit 6 bits to the left
  emit $ "    or $" ++ show falseValue ++ ", %al" -- or with the false value to return a "boolean" in the expected format

type UnaryPrim = (String, Expr -> Code)

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
             ]

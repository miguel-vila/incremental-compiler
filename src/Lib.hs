module Lib
    ( compileAndExecute
    ) where

import Control.Monad.Writer.Lazy
import System.Process
import System.IO
import System.Directory
import Data.Bits
import Data.Char(ord)
import Parser
import Expr

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

type Code = Writer [String]

emit :: String -> Code ()
emit s = tell [s]

emitProgram :: Expr -> Code ()
emitProgram = wrapInFn . emitExpr

wrapInFn :: Code () -> Code ()
wrapInFn code = do
  emit "    .text"
  emit "    .globl scheme_entry"
  emit "    .type scheme_entry, @function"
  emit "scheme_entry:"
  code
  emit "    ret"

class InmediateRepr a where
  inmediateRepr :: a -> Integer

emitExpr :: Expr -> Code ()
emitExpr (FixNum n) =
  emitLiteral $ inmediateRepr n
emitExpr (Boolean bool) =
  emitLiteral $ inmediateRepr bool
emitExpr (Character c) =
  emitLiteral $ inmediateRepr c
emitExpr Nil =
  emitLiteral 63 -- 00111111
emitExpr (UnaryFnApp name arg) =
  let (Just unaryPrim) = lookup name unaryPrims
  in unaryPrim arg

emitLiteral :: Integer -> Code ()
emitLiteral n = do
    emit ("    movl $" ++ (show n) ++ ", %eax")

toText :: Code a -> String
toText =
  unlines . snd . runWriter

unaryPrim :: Code () -> Expr -> Code ()
unaryPrim prim arg = do
  emitExpr arg
  prim

intShift :: Int
intShift = 2

instance InmediateRepr Integer where
  inmediateRepr n = n `shiftL` intShift

falseValue :: Integer
falseValue = 47  -- 00101111

trueValue :: Integer
trueValue = 111 -- 01101111

instance InmediateRepr Bool where
  inmediateRepr False = falseValue
  inmediateRepr True  = trueValue

charTag :: Int
charTag = 15

charShift :: Int
charShift = 8

instance InmediateRepr Char where
  inmediateRepr c =
    toInteger $ ord c `shiftL` charShift .|. charTag

fxadd1 :: Expr -> Code ()
fxadd1 = unaryPrim $
  emit $ "    addl $" ++ (show $ inmediateRepr (1 :: Integer)) ++ ", %eax"

charToFixNum :: Expr -> Code ()
charToFixNum = unaryPrim $
  emit $ "    sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: Expr -> Code ()
fixNumToChar = unaryPrim $ do
  emit $ "    sall $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the left 6 bits
  emit $ "    orl $" ++ show charTag ++ ", %eax" -- add char tag



type UnaryPrim = (String, Expr -> Code ())

unaryPrims :: [UnaryPrim]
unaryPrims = [ ("fxadd1", fxadd1)
             , ("char->fixnum", charToFixNum)
             , ("fixnum->char", fixNumToChar)
             ]

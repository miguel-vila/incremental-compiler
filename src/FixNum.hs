module FixNum where

import CodeGen
import MagicNumbers
import InmediateRepr
import Expr hiding (_and, _or)

fxadd1 :: FnGen
fxadd1 = UnaryFn $
  emit $ "addl $" ++ (show $ inmediateRepr $ FixNum 1) ++ ", %eax"

fxsub1 :: FnGen
fxsub1 = UnaryFn $
  emit $ "subl $" ++ (show $ inmediateRepr $ FixNum 1) ++ ", %eax"

fixNumToChar :: FnGen
fixNumToChar = UnaryFn $ do
  emit $ "sall $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the left 6 bits
  emit $ "orl $" ++ show charTag ++ ", %eax" -- add char tag

isFixnum :: FnGen
isFixnum = Predicate $ do
  applyMask $ toInteger intTag
  compareTo 0

isFxZero :: FnGen
isFxZero = Predicate $ compareTo 0

fxPlus :: FnGen
fxPlus = BinaryFn addl

fxMinus :: FnGen
fxMinus = BinaryFn $ \reg1 reg2 -> do
  subl reg2 reg1
  mov  reg1 reg2

fxLogNot :: FnGen
fxLogNot = UnaryFn $ do
  emit "not %eax"
  emit "sar $2, %eax"
  emit "sal $2, %eax"

fxLogAnd :: FnGen
fxLogAnd = BinaryFn _and

fxLogOr :: FnGen
fxLogOr = BinaryFn _or

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

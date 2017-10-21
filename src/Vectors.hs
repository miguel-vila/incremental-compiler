module Vectors where

import CodeGen
import MagicNumbers

isVector :: FnGen
isVector = Predicate $ do
  applyMask vectorMask
  compareTo (toInteger vectorTag)

vectorLength :: FnGen
vectorLength = UnaryFn $ do
  emit $ "sub $" ++ show vectorTag ++ ", %eax"
  mov "-4(%eax)" "%eax"

makeVector :: FnGen
makeVector = BinaryFn mkVector
  where mkVector n "%eax" = do
          loopLabel <- uniqueLabel
          mov n "%ebx"
          mov "%ebx" (heapPosWithOffset (-4)) -- save length
          mov "%ebp" "%ebx" -- %ebx will be iterator
          emit $ "sub $" ++ show vectorLengthOffset ++", %ebx" -- move it down after saving the length
          mov "%ebp" "%edx" -- %edx will be the limit
          emit $ "sub " ++ n ++ ", %edx" -- for each of the elements (4x)
          emit $ "sub $4, %edx" -- for the length
          emitLabel loopLabel
          emit $ "sub $4, %ebx"
          mov "%eax" "(%ebx)" -- save the element in the i-th entry
          emit $ "cmp %edx, %ebx"
          emit $ "jne " ++ loopLabel
          mov "%ebp" "%eax"
          emit $ "or $" ++ show vectorTag ++ ", %eax"
          mov "%ebx" "%ebp"

vectorRef :: FnGen
vectorRef = BinaryFn vctRef
  where vctRef vec pos = do
          mov vec "%ebx"
          emit $ "sub $" ++ show vectorTag ++ ", %ebx"
          emit $ "sub $" ++ show vectorContentOffset ++ ", %ebx"
          emit $ "sub " ++ pos ++ ", %ebx"
          mov "(%ebx)" "%eax"

vectorSet :: FnGen
vectorSet = NaryFn $ \si ->
  let vec = stackValueAt si                  -- 1st arg
      pos = stackValueAt (nextStackIndex si) -- 2nd arg
      val = "%eax"                           -- 3rd arg
  in do
    mov vec "%ebx"
    emit $ "sub $" ++ show vectorTag ++ ", %ebx"
    emit $ "sub $" ++ show vectorContentOffset ++ ", %ebx"
    emit $ "sub " ++ pos ++ ", %ebx"
    mov val "(%ebx)"

module Pairs where

import CodeGen
import MagicNumbers

isPair :: FnGen
isPair = Predicate $ do
  applyMask pairMask
  compareTo (toInteger pairTag)

cons :: FnGen
cons = BinaryFn _cons
  where _cons reg1 eax = do
          mov eax (heapPosWithOffset cdrOffset)
          mov reg1 eax
          mov eax (heapPosWithOffset carOffset)
          mov "%ebp" eax
          emit $ "orl $" ++ show pairTag ++ ", " ++ eax
          subl "$8" "%ebp"

car :: FnGen
car = UnaryFn $ do
  emit $ "subl $" ++ show pairTag ++ ", %eax"
  mov (show carOffset ++ "(%eax)") "%eax"

cdr :: FnGen
cdr = UnaryFn $ do
  emit $ "subl $" ++ show pairTag ++ ", %eax"
  mov (show cdrOffset ++ "(%eax)") "%eax"


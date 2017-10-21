module Chars where

import CodeGen
import MagicNumbers

isChar :: FnGen
isChar = Predicate $ do
    applyMask charMask
    compareTo $ toInteger charTag

charToFixNum :: FnGen
charToFixNum = UnaryFn $
  emit $ "sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

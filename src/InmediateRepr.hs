module InmediateRepr where

import MagicNumbers
import Data.Char(ord)
import Data.Bits
import Expr

inmediateRepr :: Literal -> Integer
inmediateRepr (FixNum n)      = n `shiftL` intShift
inmediateRepr (Boolean False) = falseValue
inmediateRepr (Boolean True)  = trueValue
inmediateRepr (Character c)   = toInteger $ ord c `shiftL` charShift .|. charTag
inmediateRepr Nil             = nilValue

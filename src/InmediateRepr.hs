module InmediateRepr where

import MagicNumbers
import Data.Char(ord)
import Data.Bits

class InmediateRepr a where
  inmediateRepr :: a -> Integer

instance InmediateRepr Integer where
  inmediateRepr n = n `shiftL` intShift

instance InmediateRepr Bool where
  inmediateRepr False = falseValue
  inmediateRepr True  = trueValue

instance InmediateRepr Char where
  inmediateRepr c =
    toInteger $ ord c `shiftL` charShift .|. charTag


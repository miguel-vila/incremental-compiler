module Primitives where

import Prelude hiding (lookup)
import Data.HashMap hiding (map)
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Except

import Expr
import CodeGen
import Chars
import Pairs
import Vectors
import FixNum
import MagicNumbers

lookupPrimitive :: FunctionName -> GenReaderState FnGen
lookupPrimitive primitiveName =
  maybe (throwError (FunctionNotDefined primitiveName)) return (lookup primitiveName primitives)
  
primitives :: Map FunctionName FnGen
primitives = unaryPrims `union`
  binaryPrims `union`
  naryPrims

unaryPrims :: Map FunctionName FnGen
unaryPrims = fromList [ ("fxadd1"        , fxadd1)
                      , ("fxsub1"        , fxsub1)
                      , ("char->fixnum"  , charToFixNum)
                      , ("fixnum->char"  , fixNumToChar)
                      , ("fxlognot"      , fxLogNot)
                      , ("fixnum?"       , isFixnum)
                      , ("null?"         , isNull)
                      , ("not"           , notL)
                      , ("boolean?"      , isBoolean)
                      , ("char?"         , isChar)
                      , ("fxzero?"       , isFxZero)
                      , ("pair?"         , isPair)
                      , ("car"           , car)
                      , ("cdr"           , cdr)
                      , ("vector?"       , isVector)
                      , ("vector-length" , vectorLength)
                      ]

binaryPrims :: Map FunctionName FnGen
binaryPrims = fromList [ ("fx+"         , fxPlus)
                       , ("fx-"         , fxMinus)
                       , ("fx*"         , fxTimes)
                       , ("fxlogand"    , fxLogAnd)
                       , ("fxlogor"     , fxLogOr)
                       , ("fx="         , fxEq)
                       , ("fx<"         , fxLess)
                       , ("fx<="        , fxLessOrEq)
                       , ("fx>"         , fxGreater)
                       , ("fx>="        , fxGreaterOrEq)
                       , ("cons"        , cons)
                       , ("make-vector" , makeVector)
                       , ("vector-ref"  , vectorRef)
                       ]

naryPrims :: Map FunctionName FnGen
naryPrims = fromList [ ("vector-set!" , vectorSet)
                     ]

isNull :: FnGen
isNull = Predicate $ compareTo nilValue

isBoolean :: FnGen
isBoolean = Predicate $ do
  applyMask boolMask
  compareTo falseValue

notL :: FnGen
notL = Predicate $ compareTo falseValue

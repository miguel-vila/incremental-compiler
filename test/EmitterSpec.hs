module EmitterSpec(emitterSpec) where

import Expr
import Emitter hiding (binOp)
import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.Error.Class
import Test.HUnit
import TestUtils

tabbed = (tab ++)

wrap :: Code -> Code
wrap code =
  [ tabbed ".text"
  , tabbed ".globl L_scheme_entry"
  , tabbed ".type L_scheme_entry, @function"
  , "L_scheme_entry:"
  ] ++ code ++ [
    tabbed"ret"
  , tabbed ".globl scheme_entry"
  , tabbed ".type scheme_entry, @function"
  , "scheme_entry:"
  , tabbed "mov %esp, %ecx"
  , tabbed "mov 4(%esp), %esp"
  , tabbed "call L_scheme_entry"
  , tabbed "mov %ecx, %esp"
  , tabbed "ret"
  ]


shouldEmit :: Expr -> Code -> Expectation
shouldEmit expr code =
  (compile expr) `shouldBe` (wrap code)

type TestCase = (Expr, Code)

fxPlusTests :: [TestCase]
fxPlusTests =
  [ binOp "fx+" (fx 1) (fx 2)
    ~> [ tabbed "movl $8, %eax"
       , tabbed "addl $4, %eax"
       ]
  , binOp "fx+" (binOp "fx+" (fx 1) (fx 2)) (fx 6)
    ~> [ tabbed "movl $8, %eax"
       , tabbed "addl $4, %eax"
       , tabbed "addl $24, %eax"
       ]
  , binOp "fx+" (fx 5) (binOp "fx+" (fx 1) (fx 2))
    ~> [ tabbed "movl $8, %eax"
       , tabbed "addl $4, %eax"
       , tabbed "addl $20, %eax"
       ]
  , binOp "fx+" (binOp "fx+" (fx 3) (fx 5)) (binOp "fx+" (fx 7) (fx 12))
    ~> [ tabbed "movl $20, %eax"
       , tabbed "addl $12, %eax"
       , tabbed "movl %eax, -4(%esp)"
       , tabbed "movl $48, %eax"
       , tabbed "addl $28, %eax"
       , tabbed "addl -4(%esp), %eax"
       ]
  ]

executeTestCases :: [TestCase] -> Expectation
executeTestCases = mapM_ (\(expr, expectedOutput) -> shouldEmit expr expectedOutput)

emitterSpec = describe "Compile" $ do
  it "optimizes fx+" $ executeTestCases fxPlusTests

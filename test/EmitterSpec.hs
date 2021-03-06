module EmitterSpec(emitterSpec) where

import Expr
import CodeGen
import Emitter hiding (binOp)
import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.Error.Class
import Test.HUnit
import TestUtils

wrap :: Code -> Code
wrap code =
  [ ".text"
  , ".globl L_scheme_entry"
  , ".type L_scheme_entry, @function"
  , "L_scheme_entry:"
  ] ++ code ++
  [ tabbed "ret"
  , ".globl scheme_entry"
  , ".type scheme_entry, @function"
  , "scheme_entry:"
  ]
  ++ saveRegistersInsts
  ++ loadHeapAndStackPointersInsts
  ++ [ tabbed "call L_scheme_entry" ]
  ++ restoreRegistersInsts
  ++ [ tabbed "ret" ]

exprShouldEmit :: ParsedExpr -> Code -> Expectation
exprShouldEmit expr code =
  (compile $ Expr expr) `shouldBe` (return $ wrap code)

programShouldEmit :: Program -> (Code, Code) -> Expectation
programShouldEmit program (prefix,exprCode) =
  (compile program) `shouldBe` (return $ prefix ++ wrap exprCode)

shouldErrorWith :: Program -> CompilationError -> Expectation
shouldErrorWith program error =
  (compile program) `shouldBe` (throwError error)

type ExprTestCase = (ParsedExpr, Code)

type ProgramTestCase = (Program, (Code, Code))

fxPlusTests :: [ExprTestCase]
fxPlusTests =
  [ (binOp "fx+" (fx 1) (fx 2))
    ~> [ tabbed "movl $8, %eax"
       , tabbed "addl $4, %eax"
       ]
  , (binOp "fx+" (binOp "fx+" (fx 1) (fx 2)) (fx 6))
    ~> [ tabbed "movl $8, %eax"
       , tabbed "addl $4, %eax"
       , tabbed "addl $24, %eax"
       ]
  , (binOp "fx+" (fx 5) (binOp "fx+" (fx 1) (fx 2)))
    ~> [ tabbed "movl $8, %eax"
       , tabbed "addl $4, %eax"
       , tabbed "addl $20, %eax"
       ]
  , (binOp "fx+" (binOp "fx+" (fx 3) (fx 5)) (binOp "fx+" (fx 7) (fx 12)))
    ~> [ tabbed "movl $20, %eax"
       , tabbed "addl $12, %eax"
       , tabbed "movl %eax, -4(%esp)"
       , tabbed "movl $48, %eax"
       , tabbed "addl $28, %eax"
       , tabbed "addl -4(%esp), %eax"
       ]
  ]

badFibonacci :: LambdaBinding
badFibonacci = LambdaBinding "fib" $
  Lambda ["n"] $
  _if (binOp "fx<=" (var "n") (fx 1)) (var "n") $ binOp "fx+" (app "fib" (binOp "fx-" (var "n") (fx 1))) (app "fib" (binOp "fx-" (var "n") (fx 2)))

tailRecTestCases :: [ProgramTestCase]
tailRecTestCases =
  [ LetRec [ sumFirstN1 ]
    (userFnApp "sum" [fx 10, fx 0])
    ~>
    ([ "Lambda_0:"
     , tabbed "movl -4(%esp), %eax"
     , tabbed "movl %eax, -12(%esp)"
     , tabbed "cmp $0, %eax"
     , tabbed "jne L_0"
     , tabbed "movl -8(%esp), %eax"
     , tabbed "ret"
     , "L_0:"
     , tabbed "movl -4(%esp), %eax" -- emit args
     , tabbed "subl $4, %eax"
     , tabbed "movl %eax, -16(%esp)"
     , tabbed "movl -4(%esp), %eax"
     , tabbed "addl -8(%esp), %eax"
     , tabbed "movl %eax, -20(%esp)"
     , tabbed "movl -16(%esp), %eax" -- stack colapse next 4
     , tabbed "movl %eax, -4(%esp)"
     , tabbed "movl -20(%esp), %eax"
     , tabbed "movl %eax, -8(%esp)"
     , tabbed "jmp Lambda_0"
     ],
     [ tabbed "movl $40, %eax"
     , tabbed "movl %eax, -8(%esp)"
     , tabbed "movl $0, %eax"
     , tabbed "movl %eax, -12(%esp)"
     , tabbed "addl $0, %esp"
     , tabbed "call Lambda_0"
     , tabbed "addl $0, %esp"
     ]
    )
  ]

type ErrorTestCase = (Program, CompilationError)

compilationErrorsTests :: [ErrorTestCase]
compilationErrorsTests =
  [ Expr (binOp "fx+" (fx 3) (var "x"))
    ~> VariableNotInScope "x"
  , Expr (_let ["x" <~ fx 1, "y" <~ (binOp "fx+" (var "x") (fx 2))] (var "y"))
    ~> VariableNotInScope "x"
  , Expr ( binApp "wat" (fx 1) (fx 2) )
    ~> FunctionNotDefined "wat"
  ]

executeExprTestCases :: [ExprTestCase] -> Expectation
executeExprTestCases = mapM_ (\(expr, expectedOutput) -> exprShouldEmit expr expectedOutput)

executeProgramTestCases :: [ProgramTestCase] -> Expectation
executeProgramTestCases = mapM_ (\(program, expectedOutput) -> programShouldEmit program expectedOutput)

executeErrorTestCase :: [ErrorTestCase] -> Expectation
executeErrorTestCase = mapM_ (\(program, expectedOutput) -> program `shouldErrorWith` expectedOutput)

emitterSpec = describe "Compile" $ do
  it "optimizes fx+" $ executeExprTestCases fxPlusTests
  it "optimizes proper tail calls" $ executeProgramTestCases tailRecTestCases
  it "handles errors during compilation" $ executeErrorTestCase compilationErrorsTests

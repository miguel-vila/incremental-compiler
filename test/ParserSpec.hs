module ParserSpec(parserSpec) where

import Expr
import Parser
import Test.Hspec
import Test.HUnit
import TestUtils

exprShouldBeParsedTo :: String -> Expr -> Expectation
exprShouldBeParsedTo str expr =
   (readExpr str) `shouldBe` (Right expr)

programShouldBeParsedTo :: String -> Program -> Expectation
programShouldBeParsedTo str program= (readProgram str) `shouldBe` (Right program)

type ExprTestCase = (String, Expr)

type ProgramTestCase = (String, Program)

toProgramTestCase :: ExprTestCase -> ProgramTestCase
toProgramTestCase (str, expr) = (str, Expr expr)

literalTests :: [ExprTestCase]
literalTests =
  [ "123"  ~> L (FixNum 123)
  , "#t"   ~> L (Boolean True)
  , "#f"   ~> L (Boolean False)
  , "#\\x" ~> L (Character 'x')
  , "null" ~> L Nil
  , "nil"  ~> L Nil
  , "()"   ~> L Nil
  ]

primitiveTests :: [ExprTestCase]
primitiveTests =
  [ "(fx+ 3 2)"
    ~> binOp "fx+" (fx 3) (fx 2)
  , "(fxadd1 3)"
    ~> unaryOp "fxadd1" (fx 3)
  , "(fx+ (fx- 3 2) (fxsub1 10))"
    ~> binOp "fx+" (binOp "fx-" (fx 3) (fx 2)) (unaryOp "fxsub1" (fx 10))
  ]

ifTests :: [ExprTestCase]
ifTests =
  [ "(if #t 5 6)"
    ~> If _True (fx 5) (fx 6)
  , "(if (if (fx> 2 3) 7 8) 5 6)"
    ~> If (If (binOp "fx>" (fx 2) (fx 3)) (fx 7) (fx 8)) (fx 5) (fx 6) 
  ]

andOrTests :: [ExprTestCase]
andOrTests =
  [ "(and 4 #f #\\x)"
    ~> And [fx 4, _False, L $ Character 'x']
  , "(or 3 (fx+ 3 2))"
    ~> Or [fx 3, binOp "fx+" (fx 3) (fx 2)]
  ]

varRefsTests :: [ExprTestCase]
varRefsTests =
  [ "x"
    ~> var "x"
  , "(fx+ x y)"
    ~> binOp "fx+" (var "x") (var "y")
  ]

letTests :: [ExprTestCase]
letTests =
  [ "(let ((x 3) (y (fx- 4 5))) (fx* x y))"
    ~> Let [ Binding "x" (fx 3)
           , Binding "y" (binOp "fx-" (fx 4) (fx 5))
           ] (binOp "fx*" (var "x") (var "y"))
  , "(let* ((x 3) (y (fx- x 5))) (fx* x y))"
    ~> LetStar [ Binding "x" (fx 3)
               , Binding "y" (binOp "fx-" (var "x") (fx 5))
               ] (binOp "fx*" (var "x") (var "y"))
  ]

fnAppTests :: [ExprTestCase]
fnAppTests =
  [ "(my-fn x 3)"
    ~> binApp "my-fn" (var "x") (fx 3)
  ]

programTests :: [ProgramTestCase]
programTests =
  [ "(letrec ((my-fn (lambda (x) (fx+ x 2)))) (my-fn 4))"
    ~> LetRec [LambdaBinding "my-fn" $ Lambda ["x"] (binOp "fx+" (var "x") (fx 2))] (app "my-fn" (fx 4))
  ] ++ (map toProgramTestCase (literalTests))

executeExprTestCases :: [ExprTestCase] -> Expectation
executeExprTestCases = mapM_ (\(str, expr) -> str `exprShouldBeParsedTo` expr)

executeProgramTestCases :: [ProgramTestCase] -> Expectation
executeProgramTestCases = mapM_ (\(str, program) -> str `programShouldBeParsedTo` program)

parserSpec = describe "Parser" $ do
  it "parses literals" $ executeExprTestCases literalTests
  it "parses primitive invocations" $ executeExprTestCases primitiveTests
  it "parses if expressions" $ executeExprTestCases ifTests
  it "parses and and or expressions" $ executeExprTestCases andOrTests
  it "parses var references" $ executeExprTestCases varRefsTests
  it "parses let and let* expressions" $ executeExprTestCases letTests
  it "parses user function invocations" $ executeExprTestCases fnAppTests
  it "parses programs" $ executeProgramTestCases programTests

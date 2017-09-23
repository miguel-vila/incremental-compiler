module ParserSpec(parserSpec) where

import Expr
import Parser
import Test.Hspec
import Test.HUnit
import TestUtils

shouldBeParsedTo :: String -> Expr -> Expectation
shouldBeParsedTo str expr =
   (readExpr str) `shouldBe` (Right expr)

type TestCase = (String, Expr)

literalTests :: [TestCase]
literalTests =
  [ "123"  ~> L (FixNum 123)
  , "#t"   ~> L (Boolean True)
  , "#f"   ~> L (Boolean False)
  , "#\\x" ~> L (Character 'x')
  , "null" ~> L Nil
  , "nil"  ~> L Nil
  , "()"   ~> L Nil
  ]

primitiveTests :: [TestCase]
primitiveTests =
  [ "(fx+ 3 2)"
    ~> binOp "fx+" (fx 3) (fx 2)
  , "(fxadd1 3)"
    ~> unaryOp "fxadd1" (fx 3)
  , "(fx+ (fx- 3 2) (fxsub1 10))"
    ~> binOp "fx+" (binOp "fx-" (fx 3) (fx 2)) (unaryOp "fxsub1" (fx 10))
  ]

ifTests :: [TestCase]
ifTests =
  [ "(if #t 5 6)"
    ~> If _True (fx 5) (fx 6)
  , "(if (if (fx> 2 3) 7 8) 5 6)"
    ~> If (If (binOp "fx>" (fx 2) (fx 3)) (fx 7) (fx 8)) (fx 5) (fx 6) 
  ]

andOrTests :: [TestCase]
andOrTests =
  [ "(and 4 #f #\\x)"
    ~> And [fx 4, _False, L $ Character 'x']
  , "(or 3 (fx+ 3 2))"
    ~> Or [fx 3, binOp "fx+" (fx 3) (fx 2)]
  ]

varRefsTests :: [TestCase]
varRefsTests =
  [ "x"
    ~> var "x"
  , "(fx+ x y)"
    ~> binOp "fx+" (var "x") (var "y")
  ]

letTests :: [TestCase]
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

fnAppTests :: [TestCase]
fnAppTests =
  [ "(my-fn x 3)"
    ~> binApp "my-fn" (var "x") (fx 3)
  ]

executeTestCases :: [TestCase] -> Expectation
executeTestCases = mapM_ (\(str, expr) -> str `shouldBeParsedTo` expr)

parserSpec = describe "Parser" $ do
  it "parses literals" $ executeTestCases literalTests
  it "parses primitive invocations" $ executeTestCases primitiveTests
  it "parses if expressions" $ executeTestCases ifTests
  it "parses and and or expressions" $ executeTestCases andOrTests
  it "parses var references" $ executeTestCases varRefsTests
  it "parses let and let* expressions" $ executeTestCases letTests
  it "parses user function invocations" $ executeTestCases fnAppTests

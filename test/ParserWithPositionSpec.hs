module ParserWithPositionSpec (parserWithPositionSpec) where
    
import Expr
import ParserWithPosition
import Test.Hspec
import Test.HUnit
import TestUtils

exprShouldBeParsedTo :: String -> ParsedExpr' -> Expectation
exprShouldBeParsedTo str expr =
   (readExpr' str) `shouldBe` (Right expr)

type ExprTestCase = (String, ParsedExpr')

line1FromTo :: Int -> Int -> ExprPosition
line1FromTo from to = (Position 1 from, Position 1 to)

literalTests :: [ExprTestCase]
literalTests =
  [ "123"  ~> literalP (FixNum 123) (line1FromTo 1 4)
  , "#t"   ~> literalP (Boolean True) (line1FromTo 1 3)
  , "#f"   ~> literalP (Boolean False) (line1FromTo 1 3)
  , "#\\x" ~> literalP (Character 'x') (line1FromTo 1 4)
  , "null" ~> literalP Nil (line1FromTo 1 5)
  , "nil"  ~> literalP Nil (line1FromTo 1 4)
  , "()"   ~> literalP Nil (line1FromTo 1 3)
  ]

primitiveTests :: [ExprTestCase]
primitiveTests =
  [ "(fx+ 3 2)"
    ~> binOpP "fx+" (fxP 3 (line1FromTo 6 7)) (fxP 2 (line1FromTo 8 9)) (line1FromTo 2 9)
  , "(fxadd1 3)"
    ~> unaryOpP "fxadd1" (fxP 3 (line1FromTo 9 10)) (line1FromTo 2 10)
  , "(fx+ (fx- 3 2) (fxsub1 10))"
    ~> binOpP "fx+" (binOpP "fx-" (fxP 3 (line1FromTo 11 12)) (fxP 2 (line1FromTo 13 14)) (line1FromTo 7 14)) (unaryOpP "fxsub1" (fxP 10 (line1FromTo 24 26)) (line1FromTo 17 26)) (line1FromTo 2 27)
  ]

ifTests :: [ExprTestCase]
ifTests =
  [ "(if #t 5 6)"
    ~> _ifP (_TrueP (line1FromTo 5 7)) (fxP 5 (line1FromTo 8 9)) (fxP 6 (line1FromTo 10 11)) (line1FromTo 2 11)
  , "(if (if (fx> 2 3) 7 8) 5 6)"
    ~> _ifP (_ifP (binOpP "fx>" (fxP 2 (line1FromTo 14 15)) (fxP 3 (line1FromTo 16 17)) (line1FromTo 10 17))
                  (fxP 7 (line1FromTo 19 20)) 
                  (fxP 8 (line1FromTo 21 22)) (line1FromTo 6 22)) 
            (fxP 5 (line1FromTo 24 25)) 
            (fxP 6 (line1FromTo 26 27)) (line1FromTo 2 27)
  ]

executeExprTestCases :: [ExprTestCase] -> Expectation
executeExprTestCases = mapM_ (\(str, expr) -> str `exprShouldBeParsedTo` expr)

parserWithPositionSpec = describe "ParserWithPosition" $ do
    it "parses literals" $ executeExprTestCases literalTests
    it "parses primitive invocations" $ executeExprTestCases primitiveTests
    it "parses if expressions" $ executeExprTestCases ifTests
      
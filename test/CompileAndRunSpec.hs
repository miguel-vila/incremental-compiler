module CompileAndRunSpec(compileAndRunSpec) where

import Expr
import Lib(compileAndExecute)
import Test.Hspec
import TestUtils

whenRunShouldPrint :: Expr -> String -> Expectation
whenRunShouldPrint source expectedOutput =
  compileAndExecute source `shouldReturn` expectedOutput

type TestCase = (Expr, String)

intTests :: [TestCase]
intTests = [ (fx 42) ~> "42"
           , (fx 1) ~> "1"
           , (fx $ -1) ~> "-1"
           , (fx 10) ~> "10"
           , (fx $ -10) ~> "-10"
           , (fx 536870911) ~> "536870911"
           , (fx $ -536870911) ~> "-536870911"
           ]

boolTests :: [TestCase]
boolTests = [ _False ~> "#f"
            , _True ~> "#t"
            ]

charTests :: [TestCase]
charTests = [ char 'A' ~> "#\\A"
            , char 'x' ~> "#\\x"
            , char '1' ~> "#\\1"
            , char '-' ~> "#\\-"
            , char '~' ~> "#\\~"
            , char '*' ~> "#\\*"
            , char '.' ~> "#\\."
            , char '\t' ~> "#\\\t"
            , char '"' ~> "#\\\""
            ]

unaryPrimitiveTests :: [TestCase]
unaryPrimitiveTests = [ (FnApp "fxadd1" [fx 1], "2")
                      , (FnApp "fxadd1" [fx $ -1], "0")
                      , (FnApp "fxadd1" [fx 4567], "4568")
                      , (FnApp "fxadd1" [FnApp "fxadd1" [fx 0]], "2")
                      , (FnApp "fxadd1" [FnApp "fxadd1" [FnApp "fxadd1" [FnApp "fxadd1" [FnApp "fxadd1" [fx 12]]]]], "17")
                      , (FnApp "fxsub1" [fx 4567], "4566")
                      , (FnApp "fxsub1" [fx 2], "1")
                      , (FnApp "fxsub1" [fx 1], "0")
                      , (FnApp "char->fixnum" [char 'A'], "65")
                      , (FnApp "char->fixnum" [char 'a'], "97")
                      , (FnApp "char->fixnum" [char 'z'], "122")
                      , (FnApp "char->fixnum" [char '0'], "48")
                      , (FnApp "char->fixnum" [char '9'], "57")
                      , (FnApp "fixnum->char" [fx 65], "#\\A")
                      , (FnApp "fixnum->char" [fx 97], "#\\a")
                      , (FnApp "fixnum->char" [fx 122], "#\\z")
                      , (FnApp "fixnum->char" [fx 48], "#\\0")
                      , (FnApp "fixnum->char" [fx 57], "#\\9")
                      , (FnApp "char->fixnum" [FnApp "fixnum->char" [fx 12]], "12")
                      , (FnApp "fixnum->char" [FnApp "char->fixnum" [char 'X']], "#\\X")
                      , (FnApp "fixnum?" [fx 3], "#t")
                      , (FnApp "fixnum?" [fx $ -3], "#t")
                      , (FnApp "fixnum?" [fx 0], "#t")
                      , (FnApp "fixnum?" [char 'A'], "#f")
                      , (FnApp "fixnum?" [_False], "#f")
                      , (FnApp "fixnum?" [_True], "#f")
                      , (FnApp "fixnum?" [nil], "#f")
                      , (FnApp "fxzero?" [fx 0], "#t")
                      , (FnApp "fxzero?" [fx 1], "#f")
                      , (FnApp "fxzero?" [fx 1234], "#f")
                      , (FnApp "fxzero?" [_False], "#f")
                      , (FnApp "fxzero?" [_True], "#f")
                      , (FnApp "fxzero?" [char '0'], "#f")
                      , (FnApp "null?" [nil], "#t")
                      , (FnApp "null?" [fx 123], "#f")
                      , (FnApp "null?" [fx 0], "#f")
                      , (FnApp "null?" [char 'A'], "#f")
                      , (FnApp "null?" [_False], "#f")
                      , (FnApp "null?" [_True], "#f")
                      , (FnApp "not" [_True], "#f")
                      , (FnApp "not" [_False], "#t")
                      , (FnApp "not" [fx 3], "#f")
                      , (FnApp "not" [char 'A'], "#f")
                      , (FnApp "boolean?" [_True], "#t")
                      , (FnApp "boolean?" [_False], "#t")
                      , (FnApp "boolean?" [fx 3], "#f")
                      , (FnApp "boolean?" [char 'A'], "#f")
                      , (FnApp "boolean?" [nil], "#f")
                      , (FnApp "char?" [char 'A'], "#t")
                      , (FnApp "char?" [char '0'], "#t")
                      , (FnApp "char?" [_True], "#f")
                      , (FnApp "char?" [_False], "#f")
                      , (FnApp "char?" [fx 3], "#f")
                      , (FnApp "fxlognot" [fx 0], "-1")
                      , (FnApp "fxlognot" [fx $ -1], "0")
                      , (FnApp "fxlognot" [fx 1], "-2")
                      , (FnApp "fxlognot" [fx 536870911], "-536870912")
                      , (FnApp "fxlognot" [fx $ -536870912], "536870911")
                      , (FnApp "fxlognot" [FnApp "fxlognot" [fx 237463]], "237463")
                      , (If (_True) (fx 1) (fx 2), "1")
                      , (If (FnApp "fxadd1" [fx $ -1]) (fx 123) (fx 2), "123")
                      , (If (FnApp "char?" [char 'A']) (fx 123) (fx 2), "123")
                      , (If (fx 0) (fx 1) (fx 2), "1")
                      , (If (_False) (fx 1) (fx 2), "2")
                      , (If (_True) (If (_True) (fx 1) (fx 2)) (fx 3), "1")
                      , (If (_True) (If (_False) (fx 1) (fx 2)) (fx 3), "2")
                      , (If (_False) (If (_False) (fx 1) (fx 2)) (fx 3), "3")
                      , (And [], "#f")
                      , (And [fx 3], "3")
                      , (And [fx 3, fx 2], "2")
                      , (And [fx 3, _False, fx 2], "#f")
                      , (And [_True, _True, _True], "#t")
                      , (And [_True, _True, fx 6], "6")
                      , (Or [], "#t")
                      , (Or [fx 3], "3")
                      , (Or [fx 3, fx 2], "3")
                      , (Or [_False, _False, fx 2], "2")
                      , (Or [_False, _False, _False], "#f")
                      , (Or [_False, _False, fx 6], "6")
                      ]

fxPlusTests :: [TestCase]
fxPlusTests = [ binOp "fx+" (fx 3) (fx 1) ~> "4"
              , binOp "fx+" (fx $ -1) (fx 1) ~> "0"
              , binOp "fx+" (fx $ 536870911) (fx $ -1) ~> "536870910"
              , binOp "fx+" (fx $ 536870910) (fx 1) ~> "536870911"
              , binOp "fx+" (fx $ -536870912) (fx 1) ~> "-536870911"
              , binOp "fx+" (fx $ -536870911) (fx $ -1) ~> "-536870912"
              , binOp "fx+" (fx $ 536870911) (fx $ -536870912) ~> "-1"
              , binOp "fx+" (binOp "fx+" (fx 3) (fx 5)) (fx 2) ~> "10"
              , binOp "fx+" (binOp "fx+" (fx 3) (fx 5)) (binOp "fx+" (fx 7) (fx 12)) ~> "27"
              , binOp "fx+" (binOp "fx+" (binOp "fx+" (fx 9) (fx 15)) (fx 5)) (binOp "fx+" (fx 7) (fx 12)) ~> "48"
              ]

fxMinusTests :: [TestCase]
fxMinusTests = [ binOp "fx-" (fx 5)             (fx 1) ~> "4"
               , binOp "fx-" (fx 536870910)     (fx $ -1)  ~> "536870911"
               , binOp "fx-" (fx 536870911)     (fx 1)  ~> "536870910"
               , binOp "fx-" (fx $ -536870911)  (fx 1)  ~> "-536870912"
               , binOp "fx-" (fx $ -536870912)  (fx $ -1)  ~> "-536870911"
               , binOp "fx-" (fx 1)             (fx 536870911)  ~> "-536870910"
               , binOp "fx-" (fx $ -1)          (fx 536870911)  ~> "-536870912"
               , binOp "fx-" (fx 1)             (fx $ -536870910)  ~> "536870911"
               , binOp "fx-" (fx $ -1)          (fx $ -536870912)  ~> "536870911"
               , binOp "fx-" (fx 536870911)     (fx 536870911)  ~> "0"
               , binOp "fx-" (fx 536870911)     (fx $ -536870912)  ~> "-1"
               , binOp "fx-" (fx $ -536870911)  (fx $ -536870912)  ~> "1"
               ]

fxTimesTests :: [TestCase]
fxTimesTests = [ binOp "fx*" (fx 2) (fx 3) ~> "6"
               , binOp "fx*" (fx $ -2) (fx 3) ~> "-6"
               , binOp "fx*" (fx 0) (fx 3) ~> "0"
               , binOp "fx*"
                  (binOp "fx*"
                    (binOp "fx*"
                      (binOp "fx*"
                       (binOp "fx*"
                        (fx 2)
                        (fx 3))
                       (fx 4))
                      (fx 5))
                    (fx 6))
                  (fx 7)
                  ~> "5040"
               ]

fxLogAndTests :: [TestCase]
fxLogAndTests = [ (FnApp "fxlogand" [fx $ -1, fx $ -1], "-1")
                , (FnApp "fxlogand" [fx 0, fx 1], "0")
                , (FnApp "fxlogand" [fx 5, fx 3], "1")
                , (FnApp "fxlogand" [fx 7, fx 3], "3")
                ]

fxLogNotTests :: [TestCase]
fxLogNotTests = [ (FnApp "fxlognot" [FnApp "fxlogor" [FnApp "fxlognot" [fx 7], fx 1]], "6")
                , (FnApp "fxlognot" [FnApp "fxlogor" [fx 1, FnApp "fxlognot" [fx 7]]], "6")
                ]

fxEqualsTests :: [TestCase]
fxEqualsTests = [ (FnApp "fx=" [fx 2, fx 2], "#t")
                , (FnApp "fx=" [fx 2, fx 5], "#f")
                , (FnApp "fx=" [FnApp "fx+" [fx 3, fx 2], fx 5], "#t")
                ]

fxLessTests :: [TestCase]
fxLessTests = [ (FnApp "fx<" [fx 2, fx 2], "#f")
              , (FnApp "fx<" [fx 2, fx 3], "#t")
              , (FnApp "fx<" [fx 2, fx 2], "#f")
              , (FnApp "fx<" [fx $ -2, fx 3], "#t")
              , (FnApp "fx<" [fx $ -2, fx $ -3], "#f")
              , (FnApp "fx<" [fx $ -2, fx $ -1], "#t")
              ]

fxLessOrEqTests :: [TestCase]
fxLessOrEqTests = [ (FnApp "fx<=" [fx 2, fx 3], "#t")
                  , (FnApp "fx<=" [fx 2, fx 2], "#t")
                  , (FnApp "fx<=" [fx $ -2, fx 3], "#t")
                  , (FnApp "fx<=" [fx $ -2, fx $ -3], "#f")
                  , (FnApp "fx<=" [fx $ -2, fx $ -1], "#t")
                  , (FnApp "fx<=" [ fx 12, fx 13],  "#t")
                  , (FnApp "fx<=" [ fx 12, fx 12],  "#t")
                  , (FnApp "fx<=" [ fx 13, fx 12],  "#f")
                  , (FnApp "fx<=" [ fx 16, FnApp "fx+" [ fx 13, fx 1 ] ],  "#f")
                  , (FnApp "fx<=" [ fx 16, FnApp "fx+" [ fx 13, fx 3 ] ],  "#t")
                  , (FnApp "fx<=" [ fx 16, FnApp "fx+" [ fx 13, fx 13] ],  "#t")
                  , (FnApp "fx<=" [ FnApp "fx+" [ fx 13, fx 1 ], fx 16 ],  "#t")
                  , (FnApp "fx<=" [ FnApp "fx+" [ fx 13, fx 3 ], fx 16 ],  "#t")
                  , (FnApp "fx<=" [ FnApp "fx+" [ fx 13, fx 13], fx 16 ],  "#f")
                  ]

ifComparisonTests :: [TestCase]
ifComparisonTests = [ (If (FnApp "fx<" [fx 1, fx 2]) (fx 4) (fx 7), "4")
                    , (If (FnApp "fx="  [ fx 12, fx 13 ]) (fx 12) (fx 13) , "13" )
                    , (If (FnApp "fx="  [ fx 12, fx 12 ]) (fx 13) (fx 14) , "13" )
                    , (If (FnApp "fx<"  [ fx 12, fx 13 ]) (fx 12) (fx 13) , "12" )
                    , (If (FnApp "fx<"  [ fx 12, fx 12 ]) (fx 13) (fx 14) , "14" )
                    , (If (FnApp "fx<"  [ fx 13, fx 12 ]) (fx 13) (fx 14) , "14" )
                    , (If (FnApp "fx<=" [ fx 12, fx 13 ]) (fx 12) (fx 13) , "12" )
                    , (If (FnApp "fx<=" [ fx 12, fx 12 ]) (fx 12) (fx 13) , "12" )
                    , (If (FnApp "fx<=" [ fx 13, fx 12 ]) (fx 13) (fx 14) , "14" )
                    , (If (FnApp "fx>"  [ fx 12, fx 13 ]) (fx 12) (fx 13) , "13" )
                    , (If (FnApp "fx>"  [ fx 12, fx 12 ]) (fx 12) (fx 13) , "13" )
                    , (If (FnApp "fx>"  [ fx 13, fx 12 ]) (fx 13) (fx 14) , "13" )
                    , (If (FnApp "fx>=" [ fx 12, fx 13 ]) (fx 12) (fx 13) , "13" )
                    , (If (FnApp "fx>=" [ fx 12, fx 12 ]) (fx 12) (fx 13) , "12" )
                    , (If (FnApp "fx>=" [ fx 13, fx 12 ]) (fx 13) (fx 14) , "13" )
                    ]

letExpressionTests :: [TestCase]
letExpressionTests =
  [ Let [Binding "x" (fx 3)]
    (var "x")
    ~> "3"
  , Let [Binding "x" (fx 3)]
    (binOp "fx+" (fx 7) (var "x"))
    ~> "10"
  ,Let [Binding "x" (fx 5), Binding "y" (fx 7)]
   (binOp "fx+" (var "x") (var "y"))
    ~> "12"
  , Let [Binding "x" (fx 3), Binding "y" (binOp "fx+" (fx 4) (fx 7))]
    (binOp "fx-" (var "y") (var "x"))
    ~> "8"
  , Let [Binding "x" (fx 3), Binding "y" (fx 4), Binding "z" (fx 7)]
    (binOp "fx-" (binOp "fx+" (var "z") (var "y")) (var "x"))
    ~> "8"
  , Let [Binding "x" (fx 3)]
    (Let [Binding "x" (binOp "fx+" (var "x") (var "x"))]
     (var "x"))
    ~> "6"
  , Let [Binding "x" (fx 1)]
    (Let [Binding "x" (binOp "fx+" (fx 3) (fx 4))]
     (var "x"))
    ~> "7"
  , Let [Binding "x" (binOp "fx+" (fx 1) (fx 2))]
    (Let [Binding "x" (binOp "fx+" (var "x") (fx 4))]
     (var "x"))
    ~> "7"
    , Let [Binding "x" (fx 12)]
      (Let [Binding "x" (binOp "fx+" (var "x") (var "x"))]
       (Let [Binding "x" (binOp "fx+" (var "x") (var "x"))]
        (Let [Binding "x" (binOp "fx+" (var "x") (var "x"))]
         (binOp "fx+" (var "x") (var "x"))
        )
       )
      )
      ~> "192"
    , Let [Binding "x" (fx 1)]
      (Let [Binding "x" (binOp "fx+" (var "x") (fx 1)), Binding "y" (binOp "fx+" (var "x") (fx 1))]
       (var "y"))
      ~> "2"
  ]

letStarExpressionTests :: [TestCase]
letStarExpressionTests =
  [ Let [Binding "x" (fx 1)]
      (LetStar [Binding "x" (binOp "fx+" (var "x") (fx 1)), Binding "y" (binOp "fx+" (var "x") (fx 1))]
       (var "y"))
      ~> "3"
  ]

executeTestCases :: [TestCase] -> Expectation
executeTestCases = mapM_ (\(source, expectedOutput) -> whenRunShouldPrint source expectedOutput)

compileAndRunSpec :: SpecWith ()
compileAndRunSpec = describe "CompileAndExecute" $ do
  it "evaluates number expressions" $ executeTestCases intTests
  it "evaluates boolean expressions" $ executeTestCases boolTests
  it "evaluates character expressions" $ executeTestCases charTests
  it "evaluates nil" $ nil `whenRunShouldPrint` "nil"
  it "evaluates unary primitive invocations" $ executeTestCases unaryPrimitiveTests
  it "evaluates fx+ invocations" $ executeTestCases fxPlusTests
  it "evaluates fx- invocations" $ executeTestCases fxMinusTests
  it "evaluates fx* invocations" $ executeTestCases fxTimesTests
  it "evaluates fxlogand invocations" $ executeTestCases fxLogAndTests
  it "evaluates fxlognot invocations" $ executeTestCases fxLogNotTests
  it "evaluates fx= invocations" $ executeTestCases fxEqualsTests
  it "evaluates fx< invocations" $ executeTestCases fxLessTests
  it "evaluates fx<= invocations" $ executeTestCases fxLessOrEqTests
  it "evaluates if comparisons expressions" $ executeTestCases ifComparisonTests
  it "evaluates let expressions" $ executeTestCases letExpressionTests
  it "evaluates let* expressions" $ executeTestCases letStarExpressionTests

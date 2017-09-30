module CompileAndRunSpec(compileAndRunSpec) where

import Expr
import Lib(compileAndExecute)
import Test.Hspec
import TestUtils

whenRunShouldPrint :: Program -> String -> Expectation
whenRunShouldPrint source expectedOutput =
  compileAndExecute source `shouldReturn` expectedOutput

type ExprTestCase = (Expr, String)

type LetRecTestCase = ([LambdaBinding], Expr, String)

type TestCase = (Program, String)

intTests :: [ExprTestCase]
intTests = [ (fx 42) ~> "42"
           , (fx 1) ~> "1"
           , (fx $ -1) ~> "-1"
           , (fx 10) ~> "10"
           , (fx $ -10) ~> "-10"
           , (fx 536870911) ~> "536870911"
           , (fx $ -536870911) ~> "-536870911"
           ]

boolTests :: [ExprTestCase]
boolTests = [ _False ~> "#f"
            , _True ~> "#t"
            ]

charTests :: [ExprTestCase]
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

unaryPrimitiveTests :: [ExprTestCase]
unaryPrimitiveTests = [ (PrimitiveApp "fxadd1" [fx 1], "2")
                      , (PrimitiveApp "fxadd1" [fx $ -1], "0")
                      , (PrimitiveApp "fxadd1" [fx 4567], "4568")
                      , (PrimitiveApp "fxadd1" [PrimitiveApp "fxadd1" [fx 0]], "2")
                      , (PrimitiveApp "fxadd1" [PrimitiveApp "fxadd1" [PrimitiveApp "fxadd1" [PrimitiveApp "fxadd1" [PrimitiveApp "fxadd1" [fx 12]]]]], "17")
                      , (PrimitiveApp "fxsub1" [fx 4567], "4566")
                      , (PrimitiveApp "fxsub1" [fx 2], "1")
                      , (PrimitiveApp "fxsub1" [fx 1], "0")
                      , (PrimitiveApp "char->fixnum" [char 'A'], "65")
                      , (PrimitiveApp "char->fixnum" [char 'a'], "97")
                      , (PrimitiveApp "char->fixnum" [char 'z'], "122")
                      , (PrimitiveApp "char->fixnum" [char '0'], "48")
                      , (PrimitiveApp "char->fixnum" [char '9'], "57")
                      , (PrimitiveApp "fixnum->char" [fx 65], "#\\A")
                      , (PrimitiveApp "fixnum->char" [fx 97], "#\\a")
                      , (PrimitiveApp "fixnum->char" [fx 122], "#\\z")
                      , (PrimitiveApp "fixnum->char" [fx 48], "#\\0")
                      , (PrimitiveApp "fixnum->char" [fx 57], "#\\9")
                      , (PrimitiveApp "char->fixnum" [PrimitiveApp "fixnum->char" [fx 12]], "12")
                      , (PrimitiveApp "fixnum->char" [PrimitiveApp "char->fixnum" [char 'X']], "#\\X")
                      , (PrimitiveApp "fixnum?" [fx 3], "#t")
                      , (PrimitiveApp "fixnum?" [fx $ -3], "#t")
                      , (PrimitiveApp "fixnum?" [fx 0], "#t")
                      , (PrimitiveApp "fixnum?" [char 'A'], "#f")
                      , (PrimitiveApp "fixnum?" [_False], "#f")
                      , (PrimitiveApp "fixnum?" [_True], "#f")
                      , (PrimitiveApp "fixnum?" [nil], "#f")
                      , (PrimitiveApp "fxzero?" [fx 0], "#t")
                      , (PrimitiveApp "fxzero?" [fx 1], "#f")
                      , (PrimitiveApp "fxzero?" [fx 1234], "#f")
                      , (PrimitiveApp "fxzero?" [_False], "#f")
                      , (PrimitiveApp "fxzero?" [_True], "#f")
                      , (PrimitiveApp "fxzero?" [char '0'], "#f")
                      , (PrimitiveApp "null?" [nil], "#t")
                      , (PrimitiveApp "null?" [fx 123], "#f")
                      , (PrimitiveApp "null?" [fx 0], "#f")
                      , (PrimitiveApp "null?" [char 'A'], "#f")
                      , (PrimitiveApp "null?" [_False], "#f")
                      , (PrimitiveApp "null?" [_True], "#f")
                      , (PrimitiveApp "not" [_True], "#f")
                      , (PrimitiveApp "not" [_False], "#t")
                      , (PrimitiveApp "not" [fx 3], "#f")
                      , (PrimitiveApp "not" [char 'A'], "#f")
                      , (PrimitiveApp "boolean?" [_True], "#t")
                      , (PrimitiveApp "boolean?" [_False], "#t")
                      , (PrimitiveApp "boolean?" [fx 3], "#f")
                      , (PrimitiveApp "boolean?" [char 'A'], "#f")
                      , (PrimitiveApp "boolean?" [nil], "#f")
                      , (PrimitiveApp "char?" [char 'A'], "#t")
                      , (PrimitiveApp "char?" [char '0'], "#t")
                      , (PrimitiveApp "char?" [_True], "#f")
                      , (PrimitiveApp "char?" [_False], "#f")
                      , (PrimitiveApp "char?" [fx 3], "#f")
                      , (PrimitiveApp "fxlognot" [fx 0], "-1")
                      , (PrimitiveApp "fxlognot" [fx $ -1], "0")
                      , (PrimitiveApp "fxlognot" [fx 1], "-2")
                      , (PrimitiveApp "fxlognot" [fx 536870911], "-536870912")
                      , (PrimitiveApp "fxlognot" [fx $ -536870912], "536870911")
                      , (PrimitiveApp "fxlognot" [PrimitiveApp "fxlognot" [fx 237463]], "237463")
                      , (If (_True) (fx 1) (fx 2), "1")
                      , (If (PrimitiveApp "fxadd1" [fx $ -1]) (fx 123) (fx 2), "123")
                      , (If (PrimitiveApp "char?" [char 'A']) (fx 123) (fx 2), "123")
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

fxPlusTests :: [ExprTestCase]
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

fxMinusTests :: [ExprTestCase]
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

fxTimesTests :: [ExprTestCase]
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

fxLogAndTests :: [ExprTestCase]
fxLogAndTests = [ binOp "fxlogand" (fx $ -1) (fx $ -1) ~> "-1"
                , binOp "fxlogand" (fx 0) (fx 1) ~> "0"
                , binOp "fxlogand" (fx 5) (fx 3) ~> "1"
                , binOp "fxlogand" (fx 7) (fx 3) ~> "3"
                ]

fxLogNotTests :: [ExprTestCase]
fxLogNotTests = [ (PrimitiveApp "fxlognot" [PrimitiveApp "fxlogor" [PrimitiveApp "fxlognot" [fx 7], fx 1]], "6")
                , (PrimitiveApp "fxlognot" [PrimitiveApp "fxlogor" [fx 1, PrimitiveApp "fxlognot" [fx 7]]], "6")
                ]

fxEqualsTests :: [ExprTestCase]
fxEqualsTests = [ binOp "fx=" (fx 2) (fx 2) ~> "#t"
                , binOp "fx=" (fx 2) (fx 5) ~> "#f"
                , binOp "fx=" (binOp "fx+" (fx 3) (fx 2)) (fx 5) ~> "#t"
                ]

fxLessTests :: [ExprTestCase]
fxLessTests = [ binOp "fx<" (fx 2) (fx 2) ~> "#f"
              , binOp "fx<" (fx 2) (fx 3) ~> "#t"
              , binOp "fx<" (fx 2) (fx 2) ~> "#f"
              , binOp "fx<" (fx $ -2) (fx 3) ~> "#t"
              , binOp "fx<" (fx $ -2) (fx $ -3) ~> "#f"
              , binOp "fx<" (fx $ -2) (fx $ -1) ~> "#t"
              ]

fxLessOrEqTests :: [ExprTestCase]
fxLessOrEqTests = [ binOp "fx<=" (fx 2) (fx 3) ~> "#t"
                  , binOp "fx<=" (fx 2) (fx 2) ~> "#t"
                  , binOp "fx<=" (fx $ -2) (fx 3) ~> "#t"
                  , binOp "fx<=" (fx $ -2) (fx $ -3) ~> "#f"
                  , binOp "fx<=" (fx $ -2) (fx $ -1) ~> "#t"
                  , binOp "fx<=" (fx 12) (fx 13) ~>  "#t"
                  , binOp "fx<=" (fx 12) (fx 12) ~>  "#t"
                  , binOp "fx<=" (fx 13) (fx 12) ~>  "#f"
                  , binOp "fx<=" (fx 16) (binOp "fx+" (fx 13) (fx 1) ) ~> "#f"
                  , binOp "fx<=" (fx 16) (binOp "fx+" (fx 13) (fx 3) ) ~> "#t"
                  , binOp "fx<=" (fx 16) (binOp "fx+" (fx 13) (fx 13)) ~> "#t"
                  , binOp "fx<=" (binOp "fx+" (fx 13) (fx 1) ) (fx 16) ~> "#t"
                  , binOp "fx<=" (binOp "fx+" (fx 13) (fx 3) ) (fx 16) ~> "#t"
                  , binOp "fx<=" (binOp "fx+" (fx 13) (fx 13)) (fx 16) ~> "#f"
                  ]

ifComparisonTests :: [ExprTestCase]
ifComparisonTests = [ If (binOp "fx<"  (fx 1) (fx 2)) (fx 4) (fx 7) ~> "4"
                    , If (binOp "fx="  (fx 12) (fx 13)) (fx 12) (fx 13) ~> "13"
                    , If (binOp "fx="  (fx 12) (fx 12)) (fx 13) (fx 14) ~> "13"
                    , If (binOp "fx<"  (fx 12) (fx 13)) (fx 12) (fx 13) ~> "12"
                    , If (binOp "fx<"  (fx 12) (fx 12)) (fx 13) (fx 14) ~> "14"
                    , If (binOp "fx<"  (fx 13) (fx 12)) (fx 13) (fx 14) ~> "14"
                    , If (binOp "fx<=" (fx 12) (fx 13)) (fx 12) (fx 13) ~> "12"
                    , If (binOp "fx<=" (fx 12) (fx 12)) (fx 12) (fx 13) ~> "12"
                    , If (binOp "fx<=" (fx 13) (fx 12)) (fx 13) (fx 14) ~> "14"
                    , If (binOp "fx>"  (fx 12) (fx 13)) (fx 12) (fx 13) ~> "13"
                    , If (binOp "fx>"  (fx 12) (fx 12)) (fx 12) (fx 13) ~> "13"
                    , If (binOp "fx>"  (fx 13) (fx 12)) (fx 13) (fx 14) ~> "13"
                    , If (binOp "fx>=" (fx 12) (fx 13)) (fx 12) (fx 13) ~> "13"
                    , If (binOp "fx>=" (fx 12) (fx 12)) (fx 12) (fx 13) ~> "12"
                    , If (binOp "fx>=" (fx 13) (fx 12)) (fx 13) (fx 14) ~> "13"
                    ]

letExpressionTests :: [ExprTestCase]
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

letStarExpressionTests :: [ExprTestCase]
letStarExpressionTests =
  [ Let [Binding "x" (fx 1)]
      (LetStar [Binding "x" (binOp "fx+" (var "x") (fx 1)), Binding "y" (binOp "fx+" (var "x") (fx 1))]
       (var "y"))
      ~> "3"
  ]

programTests :: [LetRecTestCase]
programTests =
  [ ([LambdaBinding "add2" $ Lambda ["x"] (PrimitiveApp "fx+" [fx 2, VarRef "x"])], UserFnApp "add2" [fx 7], "9"),
    ([], fx 2, "2"),
    ([], Let [Binding "x" (fx 5)] (binOp "fx+" (var "x") (var "x")), "10"),
    ([LambdaBinding "f" (Lambda [] (fx 5))], UserFnApp "f" [], "5"),
    ([LambdaBinding "f" (Lambda [] (fx 5))], Let [Binding "x" (UserFnApp "f" [])] (var "x"), "5"),
    ([LambdaBinding "f" (Lambda [] (fx 5))], binOp "fx+" (UserFnApp "f" []) (fx 7), "12"),
    ([LambdaBinding "f" (Lambda [] (fx 5))], binOp "fx+" (UserFnApp "f" []) (UserFnApp "f" []), "10"),
    ([ LambdaBinding "f" (Lambda [] (binOp "fx+" (fx 5) (fx 7))) ,
       LambdaBinding "g" (Lambda [] (fx 13))
     ], binOp "fx+" (UserFnApp "f" []) (UserFnApp "g" []), "25"),
    ([LambdaBinding "f" (Lambda ["x"] (binOp "fx+" (var "x") (fx 12)))], app "f" (fx 13), "25"),
    ([LambdaBinding "f" (Lambda ["x"] (binOp "fx+" (var "x") (fx 12)))], app "f" (app "f" (fx 10)), "34"),
    ([LambdaBinding "f" (Lambda ["x"] (binOp "fx+" (var "x") (fx 12)))], app "f" (app "f" (app "f" (fx 0))), "36"),
    ([ LambdaBinding "f" (Lambda ["x", "y"] (binOp "fx+" (var "x") (var "y")))
     , LambdaBinding "g" (Lambda ["x"] (binOp "fx+" (var "x") (fx 12)))], binApp "f" (fx 16) (binApp "f" (app "g" (fx 0)) (binOp "fx+" (fx 1) (app "g" (fx 0))) ), "41"),
    ([ LambdaBinding "f" (Lambda ["x"] (binApp "g" (var "x") (var "x")))
     , LambdaBinding "g" (Lambda ["x", "y"] (binOp "fx+" (var "x") (var "y")))],
      app "f" (fx 12), "24"),
    ([ LambdaBinding "e" (Lambda ["x"] (If (PrimitiveApp "fxzero?" [var "x"]) _True (app "o" (PrimitiveApp "fxsub1" [var "x"]))))
     , LambdaBinding "o" (Lambda ["x"] (If (PrimitiveApp "fxzero?" [var "x"]) _False (app "e" (PrimitiveApp "fxsub1" [var "x"])))) ],
     app "e" (fx 25), "#f"
     ),
    ([ sumFirstN1 ], (binApp "sum" (fx 10) (fx 0)), "55" ),
    ([ sumFirstN1 ], (binApp "sum" (fx 50) (fx 0)), "1275" ),
    ([ sumFirstN1 ], (binApp "sum" (fx 62) (fx 0)), "1953" ),
    ([ sumFirstN1 ], (binApp "sum" (fx 63) (fx 0)), "2016" )
--    ([ sumFirstN1 ], (binApp "sum" (fx 64) (fx 0)), "2080" ),
--    ([ sumFirstN1 ], (binApp "sum" (fx 66) (fx 0)), "2211" ),
--    ([ sumFirstN1 ], (binApp "sum" (fx 68) (fx 0)), "2346" ),
--    ([ sumFirstN1 ], (binApp "sum" (fx 70) (fx 0)), "2485" ),
--    ([ sumFirstN1 ], (binApp "sum" (fx 75) (fx 0)), "2850" )
  ]

executeTestCases :: [TestCase] -> Expectation
executeTestCases = mapM_ (\(source, expectedOutput) -> whenRunShouldPrint source expectedOutput)

executeExprTestCases :: [ExprTestCase] -> Expectation
executeExprTestCases = executeTestCases . (map (\(expr, value) -> (Expr expr, value)))

executeLetRecTestCases :: [LetRecTestCase] -> Expectation
executeLetRecTestCases = executeTestCases . (map (\(bindings, body, value) -> (LetRec bindings body, value)))

compileAndRunSpec :: SpecWith ()
compileAndRunSpec = describe "CompileAndExecute" $ do
  it "evaluates number expressions" $ executeExprTestCases intTests
  it "evaluates boolean expressions" $ executeExprTestCases boolTests
  it "evaluates character expressions" $ executeExprTestCases charTests
  it "evaluates nil" $ (Expr nil) `whenRunShouldPrint` "nil"
  it "evaluates unary primitive invocations" $ executeExprTestCases unaryPrimitiveTests
  it "evaluates fx+ invocations" $ executeExprTestCases fxPlusTests
  it "evaluates fx- invocations" $ executeExprTestCases fxMinusTests
  it "evaluates fx* invocations" $ executeExprTestCases fxTimesTests
  it "evaluates fxlogand invocations" $ executeExprTestCases fxLogAndTests
  it "evaluates fxlognot invocations" $ executeExprTestCases fxLogNotTests
  it "evaluates fx= invocations" $ executeExprTestCases fxEqualsTests
  it "evaluates fx< invocations" $ executeExprTestCases fxLessTests
  it "evaluates fx<= invocations" $ executeExprTestCases fxLessOrEqTests
  it "evaluates if comparisons expressions" $ executeExprTestCases ifComparisonTests
  it "evaluates let expressions" $ executeExprTestCases letExpressionTests
  it "evaluates let* expressions" $ executeExprTestCases letStarExpressionTests
  it "evaluates programs that define functions" $ executeLetRecTestCases programTests

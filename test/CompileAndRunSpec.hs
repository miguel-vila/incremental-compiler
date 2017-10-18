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
unaryPrimitiveTests = [ (primitiveApp "fxadd1" [fx 1], "2")
                      , (primitiveApp "fxadd1" [fx $ -1], "0")
                      , (primitiveApp "fxadd1" [fx 4567], "4568")
                      , (primitiveApp "fxadd1" [primitiveApp "fxadd1" [fx 0]], "2")
                      , (primitiveApp "fxadd1" [primitiveApp "fxadd1" [primitiveApp "fxadd1" [primitiveApp "fxadd1" [primitiveApp "fxadd1" [fx 12]]]]], "17")
                      , (primitiveApp "fxsub1" [fx 4567], "4566")
                      , (primitiveApp "fxsub1" [fx 2], "1")
                      , (primitiveApp "fxsub1" [fx 1], "0")
                      , (primitiveApp "char->fixnum" [char 'A'], "65")
                      , (primitiveApp "char->fixnum" [char 'a'], "97")
                      , (primitiveApp "char->fixnum" [char 'z'], "122")
                      , (primitiveApp "char->fixnum" [char '0'], "48")
                      , (primitiveApp "char->fixnum" [char '9'], "57")
                      , (primitiveApp "fixnum->char" [fx 65], "#\\A")
                      , (primitiveApp "fixnum->char" [fx 97], "#\\a")
                      , (primitiveApp "fixnum->char" [fx 122], "#\\z")
                      , (primitiveApp "fixnum->char" [fx 48], "#\\0")
                      , (primitiveApp "fixnum->char" [fx 57], "#\\9")
                      , (primitiveApp "char->fixnum" [primitiveApp "fixnum->char" [fx 12]], "12")
                      , (primitiveApp "fixnum->char" [primitiveApp "char->fixnum" [char 'X']], "#\\X")
                      , (primitiveApp "fixnum?" [fx 3], "#t")
                      , (primitiveApp "fixnum?" [fx $ -3], "#t")
                      , (primitiveApp "fixnum?" [fx 0], "#t")
                      , (primitiveApp "fixnum?" [char 'A'], "#f")
                      , (primitiveApp "fixnum?" [_False], "#f")
                      , (primitiveApp "fixnum?" [_True], "#f")
                      , (primitiveApp "fixnum?" [nil], "#f")
                      , (primitiveApp "fxzero?" [fx 0], "#t")
                      , (primitiveApp "fxzero?" [fx 1], "#f")
                      , (primitiveApp "fxzero?" [fx 1234], "#f")
                      , (primitiveApp "fxzero?" [_False], "#f")
                      , (primitiveApp "fxzero?" [_True], "#f")
                      , (primitiveApp "fxzero?" [char '0'], "#f")
                      , (primitiveApp "null?" [nil], "#t")
                      , (primitiveApp "null?" [fx 123], "#f")
                      , (primitiveApp "null?" [fx 0], "#f")
                      , (primitiveApp "null?" [char 'A'], "#f")
                      , (primitiveApp "null?" [_False], "#f")
                      , (primitiveApp "null?" [_True], "#f")
                      , (primitiveApp "not" [_True], "#f")
                      , (primitiveApp "not" [_False], "#t")
                      , (primitiveApp "not" [fx 3], "#f")
                      , (primitiveApp "not" [char 'A'], "#f")
                      , (primitiveApp "boolean?" [_True], "#t")
                      , (primitiveApp "boolean?" [_False], "#t")
                      , (primitiveApp "boolean?" [fx 3], "#f")
                      , (primitiveApp "boolean?" [char 'A'], "#f")
                      , (primitiveApp "boolean?" [nil], "#f")
                      , (primitiveApp "char?" [char 'A'], "#t")
                      , (primitiveApp "char?" [char '0'], "#t")
                      , (primitiveApp "char?" [_True], "#f")
                      , (primitiveApp "char?" [_False], "#f")
                      , (primitiveApp "char?" [fx 3], "#f")
                      , (primitiveApp "fxlognot" [fx 0], "-1")
                      , (primitiveApp "fxlognot" [fx $ -1], "0")
                      , (primitiveApp "fxlognot" [fx 1], "-2")
                      , (primitiveApp "fxlognot" [fx 536870911], "-536870912")
                      , (primitiveApp "fxlognot" [fx $ -536870912], "536870911")
                      , (primitiveApp "fxlognot" [primitiveApp "fxlognot" [fx 237463]], "237463")
                      , (_if (_True) (fx 1) (fx 2), "1")
                      , (_if (primitiveApp "fxadd1" [fx $ -1]) (fx 123) (fx 2), "123")
                      , (_if (primitiveApp "char?" [char 'A']) (fx 123) (fx 2), "123")
                      , (_if (fx 0) (fx 1) (fx 2), "1")
                      , (_if (_False) (fx 1) (fx 2), "2")
                      , (_if (_True) (_if (_True) (fx 1) (fx 2)) (fx 3), "1")
                      , (_if (_True) (_if (_False) (fx 1) (fx 2)) (fx 3), "2")
                      , (_if (_False) (_if (_False) (fx 1) (fx 2)) (fx 3), "3")
                      , (_and [], "#f")
                      , (_and [fx 3], "3")
                      , (_and [fx 3, fx 2], "2")
                      , (_and [fx 3, _False, fx 2], "#f")
                      , (_and [_True, _True, _True], "#t")
                      , (_and [_True, _True, fx 6], "6")
                      , (_or [], "#t")
                      , (_or [fx 3], "3")
                      , (_or [fx 3, fx 2], "3")
                      , (_or [_False, _False, fx 2], "2")
                      , (_or [_False, _False, _False], "#f")
                      , (_or [_False, _False, fx 6], "6")
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
fxLogNotTests = [ (primitiveApp "fxlognot" [primitiveApp "fxlogor" [primitiveApp "fxlognot" [fx 7], fx 1]], "6")
                , (primitiveApp "fxlognot" [primitiveApp "fxlogor" [fx 1, primitiveApp "fxlognot" [fx 7]]], "6")
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
ifComparisonTests = [ _if (binOp "fx<"  (fx 1) (fx 2)) (fx 4) (fx 7) ~> "4"
                    , _if (binOp "fx="  (fx 12) (fx 13)) (fx 12) (fx 13) ~> "13"
                    , _if (binOp "fx="  (fx 12) (fx 12)) (fx 13) (fx 14) ~> "13"
                    , _if (binOp "fx<"  (fx 12) (fx 13)) (fx 12) (fx 13) ~> "12"
                    , _if (binOp "fx<"  (fx 12) (fx 12)) (fx 13) (fx 14) ~> "14"
                    , _if (binOp "fx<"  (fx 13) (fx 12)) (fx 13) (fx 14) ~> "14"
                    , _if (binOp "fx<=" (fx 12) (fx 13)) (fx 12) (fx 13) ~> "12"
                    , _if (binOp "fx<=" (fx 12) (fx 12)) (fx 12) (fx 13) ~> "12"
                    , _if (binOp "fx<=" (fx 13) (fx 12)) (fx 13) (fx 14) ~> "14"
                    , _if (binOp "fx>"  (fx 12) (fx 13)) (fx 12) (fx 13) ~> "13"
                    , _if (binOp "fx>"  (fx 12) (fx 12)) (fx 12) (fx 13) ~> "13"
                    , _if (binOp "fx>"  (fx 13) (fx 12)) (fx 13) (fx 14) ~> "13"
                    , _if (binOp "fx>=" (fx 12) (fx 13)) (fx 12) (fx 13) ~> "13"
                    , _if (binOp "fx>=" (fx 12) (fx 12)) (fx 12) (fx 13) ~> "12"
                    , _if (binOp "fx>=" (fx 13) (fx 12)) (fx 13) (fx 14) ~> "13"
                    ]

letExpressionTests :: [ExprTestCase]
letExpressionTests =
  [ letE ["x" <~ (fx 3)]
    (var "x")
    ~> "3"
  , letE ["x" <~ (fx 3)]
    (binOp "fx+" (fx 7) (var "x"))
    ~> "10"
  ,letE ["x" <~ (fx 5), "y" <~ (fx 7)]
   (binOp "fx+" (var "x") (var "y"))
    ~> "12"
  , letE ["x" <~ (fx 3), "y" <~ (binOp "fx+" (fx 4) (fx 7))]
    (binOp "fx-" (var "y") (var "x"))
    ~> "8"
  , letE ["x" <~ (fx 3), "y" <~ (fx 4), "z" <~ (fx 7)]
    (binOp "fx-" (binOp "fx+" (var "z") (var "y")) (var "x"))
    ~> "8"
  , letE ["x" <~ (fx 3)]
    (letE ["x" <~ (binOp "fx+" (var "x") (var "x"))]
     (var "x"))
    ~> "6"
  , letE ["x" <~ (fx 1)]
    (letE ["x" <~ (binOp "fx+" (fx 3) (fx 4))]
     (var "x"))
    ~> "7"
  , letE ["x" <~ (binOp "fx+" (fx 1) (fx 2))]
    (letE ["x" <~ (binOp "fx+" (var "x") (fx 4))]
     (var "x"))
    ~> "7"
    , letE ["x" <~ (fx 12)]
      (letE ["x" <~ (binOp "fx+" (var "x") (var "x"))]
       (letE ["x" <~ (binOp "fx+" (var "x") (var "x"))]
        (letE ["x" <~ (binOp "fx+" (var "x") (var "x"))]
         (binOp "fx+" (var "x") (var "x"))
        )
       )
      )
      ~> "192"
    , letE ["x" <~ (fx 1)]
      (letE ["x" <~ (binOp "fx+" (var "x") (fx 1)), "y" <~ (binOp "fx+" (var "x") (fx 1))]
       (var "y"))
      ~> "2"
  ]

letStarExpressionTests :: [ExprTestCase]
letStarExpressionTests =
  [ letE ["x" <~ (fx 1)]
      (letStarE ["x" <~ (binOp "fx+" (var "x") (fx 1)), "y" <~ (binOp "fx+" (var "x") (fx 1))]
       (var "y"))
      ~> "3"
  ]

programTests :: [LetRecTestCase]
programTests =
  [ ([LambdaBinding "add2" $ Lambda ["x"] (primitiveApp "fx+" [fx 2, var "x"])], userFnApp "add2" [fx 7], "9"),
    ([], fx 2, "2"),
    ([], letE ["x" <~ (fx 5)] (binOp "fx+" (var "x") (var "x")), "10"),
    ([LambdaBinding "f" (Lambda [] (fx 5))], userFnApp "f" [], "5"),
    ([LambdaBinding "f" (Lambda [] (fx 5))], letE ["x" <~ (userFnApp "f" [])] (var "x"), "5"),
    ([LambdaBinding "f" (Lambda [] (fx 5))], binOp "fx+" (userFnApp "f" []) (fx 7), "12"),
    ([LambdaBinding "f" (Lambda [] (fx 5))], binOp "fx+" (userFnApp "f" []) (userFnApp "f" []), "10"),
    ([ LambdaBinding "f" (Lambda [] (binOp "fx+" (fx 5) (fx 7))) ,
       LambdaBinding "g" (Lambda [] (fx 13))
     ], binOp "fx+" (userFnApp "f" []) (userFnApp "g" []), "25"),
    ([LambdaBinding "f" (Lambda ["x"] (binOp "fx+" (var "x") (fx 12)))], app "f" (fx 13), "25"),
    ([LambdaBinding "f" (Lambda ["x"] (binOp "fx+" (var "x") (fx 12)))], app "f" (app "f" (fx 10)), "34"),
    ([LambdaBinding "f" (Lambda ["x"] (binOp "fx+" (var "x") (fx 12)))], app "f" (app "f" (app "f" (fx 0))), "36"),
    ([ LambdaBinding "f" (Lambda ["x", "y"] (binOp "fx+" (var "x") (var "y")))
     , LambdaBinding "g" (Lambda ["x"] (binOp "fx+" (var "x") (fx 12)))], binApp "f" (fx 16) (binApp "f" (app "g" (fx 0)) (binOp "fx+" (fx 1) (app "g" (fx 0))) ), "41"),
    ([ LambdaBinding "f" (Lambda ["x"] (binApp "g" (var "x") (var "x")))
     , LambdaBinding "g" (Lambda ["x", "y"] (binOp "fx+" (var "x") (var "y")))],
      app "f" (fx 12), "24"),
    ([ LambdaBinding "e" (Lambda ["x"] (_if (primitiveApp "fxzero?" [var "x"]) _True (app "o" (primitiveApp "fxsub1" [var "x"]))))
     , LambdaBinding "o" (Lambda ["x"] (_if (primitiveApp "fxzero?" [var "x"]) _False (app "e" (primitiveApp "fxsub1" [var "x"])))) ],
     app "e" (fx 25), "#f"
     ),
    ([ sumFirstN1 ], (binApp "sum" (fx 10) (fx 0)), "55" ),
    ([ sumFirstN1 ], (binApp "sum" (fx 10000) (fx 0)), "50005000" ),
    (evenOddBindings, (app "e" (fx 5000000)), "#t")
  ]

pairTests :: [ExprTestCase]
pairTests =
  [ letE [ "myPair" <~ (binOp "cons" (fx 5) (fx 10)) ]
    (unaryOp "pair?" (var "myPair"))
    ~> "#t"
  , unaryOp "pair?" (fx 3)
    ~> "#f"
  , car (cons (fx 5) (fx 7))
    ~> "5"
  , cdr (cons (fx 5) (fx 7))
    ~> "7"
  , car (cdr (cons (fx 5) (cons (fx 4) (fx 7))))
    ~> "4"
  , car (car (cons (cons (fx 3) (fx 8)) (fx 1)))
    ~> "3"
  , car (car (cons (cons (fx 12) (fx 3)) (cons _True _False)))
    ~> "12"
  , cons (fx 1) (fx 2)
    ~> "(1 . 2)"
  , letE [ "t" <~ (letE [ "t" <~ (letE [ "t" <~ (letE [ "t" <~ (cons (fx 1) (fx 2))] (var "t")) ] (var "t"))] (var "t"))] (var "t")
    ~> "(1 . 2)"
  , cons (fx 1) (cons (fx 2) (cons (fx 3) (cons (fx 4) (fx 5))))
    ~> "(1 2 3 4 . 5)"
  , letE [ "x" <~ nil ]
           (letE [ "x" <~ cons (var "x") (var "x") ]
                (letE [ "x" <~ cons (var "x") (var "x") ]
                     (letE [ "x" <~ cons (var "x") (var "x") ]
                          (cons (var "x") (var "x")))))
    ~> "((((()) ()) (()) ()) ((()) ()) (()) ())"
  , (cons (letE [ "x" <~ _True] (letE [ "y" <~ (cons (var "x") (var "x"))] (cons (var "x") (var "y"))))
        (cons (letE [ "x" <~ _False] (letE [ "y" <~ (cons (var "x") (var "x"))] (cons (var "y") (var "x"))))
         nil))
    ~> "((#t #t . #t) ((#f . #f) . #f))"
  ]

vectorTests :: [ExprTestCase]
vectorTests =
  [ unaryOp "vector?" (vector (fx 3) (fx 1))
    ~> "#t"
  , unaryOp "vector?" (fx 3)
    ~> "#f"
  , unaryOp "vector-length" (vector (fx 12) (fx 666))
    ~> "12"
  , unaryOp "vector-length" (vector (letE [ "x" <~ (binOp "fx+" (fx 4) (fx 7))
                                          , "y" <~ (fx 9)]
                                     (binOp "fx+" (var "x") (var "y"))) (fx 666))
    ~> "20"
  , vectorRef (vector (fx 3) (fx 456)) (fx 0)
    ~> "456"
  , letE [ "v" <~ (vector (fx 4) (fx 3)) ]
    (binOp "fx+"
      (binOp "fx+"
        (vectorRef (var "v") (fx 0))
        (vectorRef (var "v") (fx 1)))
      (binOp "fx+"
        (vectorRef (var "v") (fx 2))
        (vectorRef (var "v") (fx 3)))
    )
    ~> "12"
  , letE [ "v" <~ (vector (fx 5) (fx 2)) ]
    (_do [ (vectorSet (var "v") (fx 1) (fx 42))
        , (vectorRef (var "v") (fx 1))
        ])
    ~> "42"
  , letE [ "v" <~ (vector (fx 5) (fx 7)) ]
    (_do [ (vectorSet (var "v") (fx 1) (fx 42))
        , (vectorRef (var "v") (fx 0))
        ])
    ~> "7"
  , letE [ "v" <~ (vector (fx 4) (fx 6)) ]
    (_do [ (vectorSet (var "v") (fx 1) (fx 42))
        , (vectorSet (var "v") (fx 2) (fx 10))
        , (binOp "fx+"
           (binOp "fx+"
            (vectorRef (var "v") (fx 0))
            (vectorRef (var "v") (fx 1)))
           (binOp "fx+"
            (vectorRef (var "v") (fx 2))
            (vectorRef (var "v") (fx 3))))
        ])
    ~> "64"
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
  it "evaluates nil" $ (Expr nil) `whenRunShouldPrint` "()"
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
  it "evaluates pairs expressions" $ executeExprTestCases pairTests
  it "evaluates vector expressions" $ executeExprTestCases vectorTests

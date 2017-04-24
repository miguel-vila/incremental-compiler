module CompileAndRunSpec where

import Expr
import Lib(compileAndExecute)
import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.Error.Class
import Test.HUnit

whenRunShouldPrint :: Expr -> String -> Expectation
whenRunShouldPrint source expectedOutput =
  compileAndExecute source `shouldReturn` expectedOutput

type TestCase = (Expr, String)

intTests :: [TestCase]
intTests = [ ( FixNum 42, "42")
           , ( FixNum 1, "1")
           , ( FixNum $ -1, "-1")
           , ( FixNum 10, "10")
           , ( FixNum $ -10, "-10")
           , ( FixNum 536870911, "536870911")
           , ( FixNum $ -536870911, "-536870911")
           ]

boolTests :: [TestCase]
boolTests = [ ( Boolean False , "#f" )
            , ( Boolean True , "#t" )
            ]

charTests :: [TestCase]
charTests = [ ( Character 'A', "#\\A" )
            , ( Character 'x', "#\\x" )
            , ( Character '1', "#\\1" )
            , ( Character '-', "#\\-" )
            , ( Character '~', "#\\~" )
            , ( Character '*', "#\\*" )
            , ( Character '.', "#\\." )
            , ( Character '\t', "#\\\t" )
            , ( Character '"', "#\\\"" )
            ]

unaryPrimitiveTests :: [TestCase]
unaryPrimitiveTests = [ (FnApp "fxadd1" [FixNum 1], "2")
                      , (FnApp "fxadd1" [FixNum $ -1], "0")
                      , (FnApp "fxadd1" [FixNum 4567], "4568")
                      , (FnApp "fxadd1" [FnApp "fxadd1" [FixNum 0]], "2")
                      , (FnApp "fxadd1" [FnApp "fxadd1" [FnApp "fxadd1" [FnApp "fxadd1" [FnApp "fxadd1" [FixNum 12]]]]], "17")
                      , (FnApp "fxsub1" [FixNum 4567], "4566")
                      , (FnApp "fxsub1" [FixNum 2], "1")
                      , (FnApp "fxsub1" [FixNum 1], "0")
                      , (FnApp "char->fixnum" [Character 'A'], "65")
                      , (FnApp "char->fixnum" [Character 'a'], "97")
                      , (FnApp "char->fixnum" [Character 'z'], "122")
                      , (FnApp "char->fixnum" [Character '0'], "48")
                      , (FnApp "char->fixnum" [Character '9'], "57")
                      , (FnApp "fixnum->char" [FixNum 65], "#\\A")
                      , (FnApp "fixnum->char" [FixNum 97], "#\\a")
                      , (FnApp "fixnum->char" [FixNum 122], "#\\z")
                      , (FnApp "fixnum->char" [FixNum 48], "#\\0")
                      , (FnApp "fixnum->char" [FixNum 57], "#\\9")
                      , (FnApp "char->fixnum" [FnApp "fixnum->char" [FixNum 12]], "12")
                      , (FnApp "fixnum->char" [FnApp "char->fixnum" [Character 'X']], "#\\X")
                      , (FnApp "fixnum?" [FixNum 3], "#t")
                      , (FnApp "fixnum?" [FixNum $ -3], "#t")
                      , (FnApp "fixnum?" [FixNum 0], "#t")
                      , (FnApp "fixnum?" [Character 'A'], "#f")
                      , (FnApp "fixnum?" [Boolean False], "#f")
                      , (FnApp "fixnum?" [Boolean True], "#f")
                      , (FnApp "fixnum?" [Nil], "#f")
                      , (FnApp "fxzero?" [FixNum 0], "#t")
                      , (FnApp "fxzero?" [FixNum 1], "#f")
                      , (FnApp "fxzero?" [FixNum 1234], "#f")
                      , (FnApp "fxzero?" [Boolean False], "#f")
                      , (FnApp "fxzero?" [Boolean True], "#f")
                      , (FnApp "fxzero?" [Character '0'], "#f")
                      , (FnApp "null?" [Nil], "#t")
                      , (FnApp "null?" [FixNum 123], "#f")
                      , (FnApp "null?" [FixNum 0], "#f")
                      , (FnApp "null?" [Character 'A'], "#f")
                      , (FnApp "null?" [Boolean False], "#f")
                      , (FnApp "null?" [Boolean True], "#f")
                      , (FnApp "not" [Boolean True], "#f")
                      , (FnApp "not" [Boolean False], "#t")
                      , (FnApp "not" [FixNum 3], "#f")
                      , (FnApp "not" [Character 'A'], "#f")
                      , (FnApp "boolean?" [Boolean True], "#t")
                      , (FnApp "boolean?" [Boolean False], "#t")
                      , (FnApp "boolean?" [FixNum 3], "#f")
                      , (FnApp "boolean?" [Character 'A'], "#f")
                      , (FnApp "boolean?" [Nil], "#f")
                      , (FnApp "char?" [Character 'A'], "#t")
                      , (FnApp "char?" [Character '0'], "#t")
                      , (FnApp "char?" [Boolean True], "#f")
                      , (FnApp "char?" [Boolean False], "#f")
                      , (FnApp "char?" [FixNum 3], "#f")
                      , (FnApp "fxlognot" [FixNum 0], "-1")
                      , (FnApp "fxlognot" [FixNum $ -1], "0")
                      , (FnApp "fxlognot" [FixNum 1], "-2")
                      , (FnApp "fxlognot" [FixNum 536870911], "-536870912")
                      , (FnApp "fxlognot" [FixNum $ -536870912], "536870911")
                      , (FnApp "fxlognot" [FnApp "fxlognot" [FixNum 237463]], "237463")
                      , (If (Boolean True) (FixNum 1) (FixNum 2), "1")
                      , (If (FnApp "fxadd1" [FixNum $ -1]) (FixNum 123) (FixNum 2), "123")
                      , (If (FnApp "char?" [Character 'A']) (FixNum 123) (FixNum 2), "123")
                      , (If (FixNum 0) (FixNum 1) (FixNum 2), "1")
                      , (If (Boolean False) (FixNum 1) (FixNum 2), "2")
                      , (If (Boolean True) (If (Boolean True) (FixNum 1) (FixNum 2)) (FixNum 3), "1")
                      , (If (Boolean True) (If (Boolean False) (FixNum 1) (FixNum 2)) (FixNum 3), "2")
                      , (If (Boolean False) (If (Boolean False) (FixNum 1) (FixNum 2)) (FixNum 3), "3")
                      , (And [], "#f")
                      , (And [FixNum 3], "3")
                      , (And [FixNum 3, FixNum 2], "2")
                      , (And [FixNum 3, Boolean False, FixNum 2], "#f")
                      , (And [Boolean True, Boolean True, Boolean True], "#t")
                      , (And [Boolean True, Boolean True, FixNum 6], "6")
                      , (Or [], "#t")
                      , (Or [FixNum 3], "3")
                      , (Or [FixNum 3, FixNum 2], "3")
                      , (Or [Boolean False, Boolean False, FixNum 2], "2")
                      , (Or [Boolean False, Boolean False, Boolean False], "#f")
                      , (Or [Boolean False, Boolean False, FixNum 6], "6")
                      ]

binaryPrimitiveTests :: [TestCase]
binaryPrimitiveTests = [ (FnApp "fx+" [FixNum 3, FixNum 1], "4")
                       , (FnApp "fx+" [FixNum $ -1, FixNum 1], "0")
                       , (FnApp "fx+" [FnApp "fx+" [FixNum 3, FixNum 5], FixNum 2], "10")
                       , (FnApp "fx+" [FnApp "fx+" [FixNum 3, FixNum 5], FnApp "fx+" [FixNum 7, FixNum 12]], "27")
                       , (FnApp "fx+" [FnApp "fx+" [FnApp "fx+" [FixNum 9, FixNum 15], FixNum 5], FnApp "fx+" [FixNum 7, FixNum 12]], "48")
                       , (FnApp "fx-" [FixNum 5, FixNum 1], "4")
                       , (FnApp "fxlogand" [FixNum $ -1, FixNum $ -1], "-1")
                       , (FnApp "fxlogand" [FixNum 0, FixNum 1], "0")
                       , (FnApp "fxlogand" [FixNum 5, FixNum 3], "1")
                       , (FnApp "fx=" [FixNum 2, FixNum 2], "#t")
                       , (FnApp "fx=" [FixNum 2, FixNum 5], "#f")
                       , (FnApp "fx=" [FnApp "fx+" [FixNum 3, FixNum 2], FixNum 5], "#t")
                       , (FnApp "fx<" [FixNum 2, FixNum 2], "#f")
                       , (FnApp "fx<" [FixNum 2, FixNum 3], "#t")
                       , (FnApp "fx<" [FixNum 2, FixNum 2], "#f")
                       , (FnApp "fx<" [FixNum $ -2, FixNum 3], "#t")
                       , (FnApp "fx<" [FixNum $ -2, FixNum $ -3], "#f")
                       , (FnApp "fx<" [FixNum $ -2, FixNum $ -1], "#t")
                       , (FnApp "fx<=" [FixNum 2, FixNum 3], "#t")
                       , (FnApp "fx<=" [FixNum 2, FixNum 2], "#t")
                       , (FnApp "fx<=" [FixNum $ -2, FixNum 3], "#t")
                       , (FnApp "fx<=" [FixNum $ -2, FixNum $ -3], "#f")
                       , (FnApp "fx<=" [FixNum $ -2, FixNum $ -1], "#t")
                       , (If (FnApp "fx<" [FixNum 1, FixNum 2]) (FixNum 4) (FixNum 7), "4")
                       ]

executeTestCases :: [TestCase] -> Expectation
executeTestCases = mapM_ (\(source, expectedOutput) -> whenRunShouldPrint source expectedOutput)

compileAndRunSpec = describe "CompileAndExecute" $ do
  it "evaluates number expressions" $ executeTestCases intTests
  it "evaluates boolean expressions" $ executeTestCases boolTests
  it "evaluates character expressions" $ executeTestCases charTests
  it "evaluates nil" $ Nil `whenRunShouldPrint` "nil"
  it "evaluates unary primitive invocations" $ executeTestCases unaryPrimitiveTests
  it "evaluates binary primitive invocations" $ executeTestCases binaryPrimitiveTests

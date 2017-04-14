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

primitiveTests :: [TestCase]
primitiveTests = [ (UnaryFnApp "fxadd1" (FixNum 1), "2")
                 , (UnaryFnApp "fxadd1" (FixNum $ -1), "0")
                 , (UnaryFnApp "fxadd1" (FixNum 4567), "4568")
                 , (UnaryFnApp "fxadd1" (UnaryFnApp "fxadd1" (FixNum 0)), "2")
                 , (UnaryFnApp "fxadd1" (UnaryFnApp "fxadd1" (UnaryFnApp "fxadd1" (UnaryFnApp "fxadd1" (UnaryFnApp "fxadd1" (FixNum 12))))), "17")
                 , (UnaryFnApp "fxsub1" (FixNum 4567), "4566")
                 , (UnaryFnApp "fxsub1" (FixNum 2), "1")
                 , (UnaryFnApp "fxsub1" (FixNum 1), "0")
                 , (UnaryFnApp "char->fixnum" (Character 'A'), "65")
                 , (UnaryFnApp "char->fixnum" (Character 'a'), "97")
                 , (UnaryFnApp "char->fixnum" (Character 'z'), "122")
                 , (UnaryFnApp "char->fixnum" (Character '0'), "48")
                 , (UnaryFnApp "char->fixnum" (Character '9'), "57")
                 , (UnaryFnApp "fixnum->char" (FixNum 65), "#\\A")
                 , (UnaryFnApp "fixnum->char" (FixNum 97), "#\\a")
                 , (UnaryFnApp "fixnum->char" (FixNum 122), "#\\z")
                 , (UnaryFnApp "fixnum->char" (FixNum 48), "#\\0")
                 , (UnaryFnApp "fixnum->char" (FixNum 57), "#\\9")
                 , (UnaryFnApp "char->fixnum" (UnaryFnApp "fixnum->char" (FixNum 12)), "12")
                 , (UnaryFnApp "fixnum->char" (UnaryFnApp "char->fixnum" (Character 'X')), "#\\X")
                 , (UnaryFnApp "fixnum?" (FixNum 3), "#t")
                 , (UnaryFnApp "fixnum?" (FixNum $ -3), "#t")
                 , (UnaryFnApp "fixnum?" (FixNum 0), "#t")
                 , (UnaryFnApp "fixnum?" (Character 'A'), "#f")
                 , (UnaryFnApp "fixnum?" (Boolean False), "#f")
                 , (UnaryFnApp "fixnum?" (Boolean True), "#f")
                 , (UnaryFnApp "fixnum?" Nil, "#f")
                 , (UnaryFnApp "fxzero?" (FixNum 0), "#t")
                 , (UnaryFnApp "fxzero?" (FixNum 1), "#f")
                 , (UnaryFnApp "fxzero?" (FixNum 1234), "#f")
                 , (UnaryFnApp "fxzero?" (Boolean False), "#f")
                 , (UnaryFnApp "fxzero?" (Boolean True), "#f")
                 , (UnaryFnApp "fxzero?" (Character '0'), "#f")
                 , (UnaryFnApp "null?" Nil, "#t")
                 , (UnaryFnApp "null?" (FixNum 123), "#f")
                 , (UnaryFnApp "null?" (FixNum 0), "#f")
                 , (UnaryFnApp "null?" (Character 'A'), "#f")
                 , (UnaryFnApp "null?" (Boolean False), "#f")
                 , (UnaryFnApp "null?" (Boolean True), "#f")
                 , (UnaryFnApp "not" (Boolean True), "#f")
                 , (UnaryFnApp "not" (Boolean False), "#t")
                 , (UnaryFnApp "not" (FixNum 3), "#f")
                 , (UnaryFnApp "not" (Character 'A'), "#f")
                 , (UnaryFnApp "boolean?" (Boolean True), "#t")
                 , (UnaryFnApp "boolean?" (Boolean False), "#t")
                 , (UnaryFnApp "boolean?" (FixNum 3), "#f")
                 , (UnaryFnApp "boolean?" (Character 'A'), "#f")
                 , (UnaryFnApp "boolean?" Nil, "#f")
                 , (UnaryFnApp "char?" (Character 'A'), "#t")
                 , (UnaryFnApp "char?" (Character '0'), "#t")
                 , (UnaryFnApp "char?" (Boolean True), "#f")
                 , (UnaryFnApp "char?" (Boolean False), "#f")
                 , (UnaryFnApp "char?" (FixNum 3), "#f")
                 , (UnaryFnApp "fxlognot" (FixNum 0), "-1")
                 , (UnaryFnApp "fxlognot" (FixNum $ -1), "0")
                 , (UnaryFnApp "fxlognot" (FixNum 1), "-2")
                 , (UnaryFnApp "fxlognot" (FixNum 536870911), "-536870912")
                 , (UnaryFnApp "fxlognot" (FixNum $ -536870912), "536870911")
                 , (UnaryFnApp "fxlognot" (UnaryFnApp "fxlognot" (FixNum 237463)), "237463")
                 , (If (Boolean True) (FixNum 1) (FixNum 2), "1")
                 , (If (UnaryFnApp "fxadd1" (FixNum $ -1)) (FixNum 123) (FixNum 2), "123")
                 , (If (UnaryFnApp "char?" (Character 'A')) (FixNum 123) (FixNum 2), "123")
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

executeTestCases :: [TestCase] -> Expectation
executeTestCases = mapM_ (\(source, expectedOutput) -> whenRunShouldPrint source expectedOutput)

compileAndRunSpec = describe "CompileAndExecute" $ do
  it "evaluates number expressions" $ executeTestCases intTests
  it "evaluates boolean expressions" $ executeTestCases boolTests
  it "evaluates character expressions" $ executeTestCases charTests
  it "evaluates nil" $ Nil `whenRunShouldPrint` "nil"
  it "evaluates unary primitive invocations" $ executeTestCases primitiveTests


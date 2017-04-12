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
                 ]

executeTestCases :: [TestCase] -> Expectation
executeTestCases = mapM_ (\(source, expectedOutput) -> whenRunShouldPrint source expectedOutput)

compileAndRunSpec = describe "CompileAndExecute" $ do
  it "evaluates number expressions" $ executeTestCases intTests
  it "evaluates boolean expressions" $ executeTestCases boolTests
  it "evaluates character expressions" $ executeTestCases charTests
  it "evaluates nil" $ Nil `whenRunShouldPrint` "nil"
  it "evaluates unary primitive invocations" $ executeTestCases primitiveTests

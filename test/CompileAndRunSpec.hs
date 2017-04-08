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
                 , (UnaryFnApp "fxsub1" (FixNum 4567), "4566")
                 , (UnaryFnApp "fxsub1" (FixNum 2), "1")
                 , (UnaryFnApp "fxsub1" (FixNum 1), "0")
                 , (UnaryFnApp "char->fixnum" (Character 'A'), "65")
                 , (UnaryFnApp "fixnum->char" (FixNum 65), "#\\A")
                 , (UnaryFnApp "fixnum?" (FixNum 3), "#t")
                 , (UnaryFnApp "fixnum?" (FixNum $ -3), "#t")
                 , (UnaryFnApp "fixnum?" (FixNum 0), "#t")
                 , (UnaryFnApp "fixnum?" (Character 'A'), "#f")
                 , (UnaryFnApp "fixnum?" (Boolean False), "#f")
                 , (UnaryFnApp "fixnum?" (Boolean True), "#f")
                 , (UnaryFnApp "fixnum?" Nil, "#f")
                 ]

executeTestCases :: [TestCase] -> Expectation
executeTestCases = mapM_ (\(source, expectedOutput) -> whenRunShouldPrint source expectedOutput)

compileAndRunSpec = describe "CompileAndExecute" $ do
  it "evaluates number expressions" $ executeTestCases intTests
  it "evaluates boolean expressions" $ executeTestCases boolTests
  it "evaluates character expressions" $ executeTestCases charTests
  it "evaluates nil" $ Nil `whenRunShouldPrint` "nil"
  it "evaluates unary primitive invocations" $ executeTestCases primitiveTests

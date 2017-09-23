module ParserSpec(parserSpec) where

import Expr
import Parser
import Test.Hspec
import Test.HUnit
import TestUtils

shouldBeParsedTo :: String -> Expr -> Expectation
shouldBeParsedTo str expr =
   (readExpr str) `shouldBe` (Right expr)

parserSpec = describe "Parser" $ do
  it "parses literals" $
    "123" `shouldBeParsedTo` (L $ FixNum 123)

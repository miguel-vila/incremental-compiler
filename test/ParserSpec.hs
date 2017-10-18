module ParserSpec(parserSpec) where

import Expr
import Parser
import Test.Hspec
import Test.HUnit
import TestUtils

exprShouldBeParsedTo :: String -> Expr -> Expectation
exprShouldBeParsedTo str expr =
   (readExpr str) `shouldBe` (Right expr)

programShouldBeParsedTo :: String -> Program -> Expectation
programShouldBeParsedTo str program= (readProgram str) `shouldBe` (Right program)

type ExprTestCase = (String, Expr)

type ProgramTestCase = (String, Program)

toProgramTestCase :: ExprTestCase -> ProgramTestCase
toProgramTestCase (str, expr) = (str, Expr expr)

literalTests :: [ExprTestCase]
literalTests =
  [ "123"  ~> literal (FixNum 123)
  , "#t"   ~> literal (Boolean True)
  , "#f"   ~> literal (Boolean False)
  , "#\\x" ~> literal (Character 'x')
  , "null" ~> literal Nil
  , "nil"  ~> literal Nil
  , "()"   ~> literal Nil
  ]

primitiveTests :: [ExprTestCase]
primitiveTests =
  [ "(fx+ 3 2)"
    ~> binOp "fx+" (fx 3) (fx 2)
  , "(fxadd1 3)"
    ~> unaryOp "fxadd1" (fx 3)
  , "(fx+ (fx- 3 2) (fxsub1 10))"
    ~> binOp "fx+" (binOp "fx-" (fx 3) (fx 2)) (unaryOp "fxsub1" (fx 10))
  ]

ifTests :: [ExprTestCase]
ifTests =
  [ "(if #t 5 6)"
    ~> _if _True (fx 5) (fx 6)
  , "(if (if (fx> 2 3) 7 8) 5 6)"
    ~> _if (_if (binOp "fx>" (fx 2) (fx 3)) (fx 7) (fx 8)) (fx 5) (fx 6)
  ]

andOrTests :: [ExprTestCase]
andOrTests =
  [ "(and 4 #f #\\x)"
    ~> _and [fx 4, _False, char 'x']
  , "(or 3 (fx+ 3 2))"
    ~> _or [fx 3, binOp "fx+" (fx 3) (fx 2)]
  ]

varRefsTests :: [ExprTestCase]
varRefsTests =
  [ "x"
    ~> var "x"
  , "(fx+ x y)"
    ~> binOp "fx+" (var "x") (var "y")
  ]

letTests :: [ExprTestCase]
letTests =
  [ "(let [(x 3) (y (fx- 4 5))] (fx* x y))"
    ~> _let [ "x" ~> (fx 3)
           , "y" ~> (binOp "fx-" (fx 4) (fx 5))
           ] (binOp "fx*" (var "x") (var "y"))
  , "(let ((x 3) (y (fx- 4 5))) (fx* x y))"
    ~> _let [ "x" ~> (fx 3)
           , "y" ~> (binOp "fx-" (fx 4) (fx 5))
           ] (binOp "fx*" (var "x") (var "y"))
  , "(let* ((x 3) (y (fx- x 5))) (fx* x y))"
    ~> _letStar [ "x" ~> (fx 3)
                , "y" ~> (binOp "fx-" (var "x") (fx 5))
                ] (binOp "fx*" (var "x") (var "y"))
  , "(let ([t (let ([t (let ([t (let ([t (cons 1 2)]) t)]) t)]) t)]) t)"
    ~> _let [ "t" ~> (_let [ "t" ~> (_let [ "t" ~> (_let [ "t" ~> (cons (fx 1) (fx 2))] (var "t")) ] (var "t"))] (var "t"))] (var "t")
  , concat [ "(let ([x ()])"
           , "     (let ([x (cons x x)])"
           , "            (let ([x (cons x x)])"
           , "                     (let ([x (cons x x)])"
           , "                                (cons x x)))))" ]
    ~> _let [ "x" ~> nil]
           (_let [ "x" ~> cons (var "x") (var "x") ]
                (_let [ "x" ~> cons (var "x") (var "x")]
                     (_let [ "x" ~> cons (var "x") (var "x") ]
                          (cons (var "x") (var "x")))))
  , concat [ "(cons (let ([x #t]) (let ([y (cons x x)]) (cons x y)))"
           , "         (cons (let ([x #f]) (let ([y (cons x x)]) (cons y x)))"
           , "                        ()))" ]
    ~> (cons (_let ["x" ~> _True] (_let ["y" ~> (cons (var "x") (var "x"))] (cons (var "x") (var "y"))))
        (cons (_let ["x" ~> _False] (_let ["y" ~> (cons (var "x") (var "x"))] (cons (var "y") (var "x"))))
         nil))
  ]

fnAppTests :: [ExprTestCase]
fnAppTests =
  [ "(my-fn)"
    ~> (userFnApp "my-fn" [])
  , "(f)"
    ~> (userFnApp "f" [])
  , "(my-fn x 3)"
    ~> binApp "my-fn" (var "x") (fx 3)
  ]

doTests :: [ExprTestCase]
doTests =
  [ (unlines [ "(let ([v (make-vector 5 2)])"
             , "  (do (vector-set! v 1 42)"
             , "      (vector-ref v 1)))"
             ])
    ~> (_let [ "v" <~ (vector (fx 5) (fx 2)) ]
        (_do [ (vectorSet (var "v") (fx 1) (fx 42))
            , (vectorRef (var "v") (fx 1))
            ]))
  ]

programTests :: [ProgramTestCase]
programTests =
  [ "(letrec ((my-fn (lambda (x) (fx+ x 2)))) (my-fn 4))"
    ~> LetRec [LambdaBinding "my-fn" $ Lambda ["x"] (binOp "fx+" (var "x") (fx 2))] (app "my-fn" (fx 4))
  , (unlines [ "(letrec ((sum (lambda (n acc)"
             , "                (if (fxzero? n)"
             , "                    acc"
             , "                    (sum (fxsub1 n) (fx+ n acc))))))"
             , "  (sum 10000 0))"
             ]) ~> LetRec [sumFirstN1] (binApp "sum" (fx 10000) (fx 0))
  , (unlines [ "(letrec ((e (lambda (x) (if (fxzero? x) #t (o (fxsub1 x)))))"
             , "         (o (lambda (x) (if (fxzero? x) #f (e (fxsub1 x))))))"
             , "     (e 5000000))"]) ~>
    LetRec evenOddBindings (app "e" (fx 5000000))
  ] ++ (map toProgramTestCase (literalTests))

executeExprTestCases :: [ExprTestCase] -> Expectation
executeExprTestCases = mapM_ (\(str, expr) -> str `exprShouldBeParsedTo` expr)

executeProgramTestCases :: [ProgramTestCase] -> Expectation
executeProgramTestCases = mapM_ (\(str, program) -> str `programShouldBeParsedTo` program)

parserSpec = describe "Parser" $ do
  it "parses literals" $ executeExprTestCases literalTests
  it "parses primitive invocations" $ executeExprTestCases primitiveTests
  it "parses if expressions" $ executeExprTestCases ifTests
  it "parses and and or expressions" $ executeExprTestCases andOrTests
  it "parses var references" $ executeExprTestCases varRefsTests
  it "parses let and let* expressions" $ executeExprTestCases letTests
  it "parses user function invocations" $ executeExprTestCases fnAppTests
  it "parses do invocations" $ executeExprTestCases doTests
  it "parses programs" $ executeProgramTestCases programTests

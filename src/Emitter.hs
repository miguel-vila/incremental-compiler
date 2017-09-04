module Emitter where

import Prelude hiding (lookup)
import Control.Monad.Writer.Lazy
import MagicNumbers
import Expr
import InmediateRepr
import Control.Monad.State.Lazy
import Data.HashMap

type Code = [String]

-- The id for the function labels
type CodeGenState = Int

type GenState = StateT CodeGenState (Writer Code)

type CodeGen = GenState ()

data FnGen = UnaryFn CodeGen
           | BinaryFn (Register -> Register -> CodeGen)
           | Predicate CodeGen
           | Comparison ComparisonType (Register -> CodeGen)

data Environment = Environment
  { varEnv :: Map VarName StackIndex
  , fnEnv  :: Map VarName Label
  }

type FnName = String

type FnEnvironment = Map FnName Label

wordsize :: Integer
wordsize = 4

initialState :: CodeGenState
initialState = 0

initialStackIndex :: StackIndex
initialStackIndex = - wordsize

initialEnvironment :: Environment
initialEnvironment = Environment empty empty

compile :: Program -> Code
compile = executeGen . emitProgram

executeGen :: CodeGen -> Code
executeGen codeGen = execWriter $ evalStateT codeGen initialState

tab :: String
tab = "    "

emit :: String -> CodeGen
emit s = tell [ tab ++ s]

emitNoTab :: String -> CodeGen
emitNoTab s = tell [s]

noop :: CodeGen
noop = tell []

emitFunctionHeader :: Label -> CodeGen
emitFunctionHeader label = do
  emitNoTab $ ".globl " ++ label
  emitNoTab $ ".type " ++ label ++ ", @function"
  emitLabel label

wrapInEntryPoint :: CodeGen -> CodeGen
wrapInEntryPoint code = do
  emitNoTab ".text"
  emitFunctionHeader "L_scheme_entry"
  code
  emit "ret"
  emitFunctionHeader "scheme_entry"
  emit "mov %esp, %ecx"      -- save C's stack pointer
  emit "mov 4(%esp), %esp"   -- load stack pointer from parameter
  emit "call L_scheme_entry"
  emit "mov %ecx, %esp"      -- restore C's stack pointer
  emit "ret"

emitLiteral :: Integer -> CodeGen
emitLiteral n = do
  emit $ "movl $" ++ show n ++ ", %eax"

applyMask :: Integer -> CodeGen
applyMask mask =
  emit $ "and $" ++ show mask ++ ", %al"

emitBooleanByComparison :: ComparisonType -> CodeGen
emitBooleanByComparison compareType = do
  emit $ comparisonToSet compareType ++ " %al"  -- set %al to the result of the comparison
  emit $ "movzbl %al, %eax"                   -- mov %al to %eax and pad the remaining bits with 0: https://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Move_with_zero_extend --> why is this needed?
  emit $ "sal $6, %al"                        -- move the result bit 6 bits to the left
  emit $ "or $" ++ show falseValue ++ ", %al" -- or with the false value to return a "boolean" in the expected format

ifEqReturnTrue :: CodeGen
ifEqReturnTrue = emitBooleanByComparison Eq

emitExpr :: Environment -> StackIndex -> Expr -> CodeGen
emitExpr _ _ (L literal) =
  emitLiteral $ inmediateRepr literal
emitExpr env si (FnApp name args) =
  let (Just primitive) = lookup name primitives  -- @TODO handle this
  in emitFnApp name env si primitive args
emitExpr env si (If condition conseq altern) =
  emitIf env si condition conseq altern
emitExpr env si (And preds) =
  emitAnd env si preds
emitExpr env si (Or preds) =
  emitOr env si preds
emitExpr env si (Let bindings body) =
  emitLet env si bindings body
emitExpr env si (LetStar bindings body) =
  emitLetStar env si bindings body
emitExpr env _ (VarRef varName) =
  let Just si = lookup varName (varEnv env) -- @TODO handle this
  in emitStackLoad si
emitExpr env si (UserFnApp fnName args) =
  emitUserFnApp env si fnName args
emitExpr _ _ NoOp =
  noop

insertVarBinding :: VarName -> StackIndex -> Environment -> Environment
insertVarBinding varName si env =
  env { varEnv = insert varName si (varEnv env) }

emitAdjustBase :: Integer -> CodeGen
emitAdjustBase n =
  emit $ "addl $" ++ show n ++ ", %esp"

emitUserFnApp :: Environment -> StackIndex -> FunctionName -> [Expr] -> CodeGen
emitUserFnApp env si fnName args =
  let Just label = lookup fnName (fnEnv env) -- @TODO handle this
  in do emitArgs env (nextStackIndex si) args
        emitAdjustBase (si + wordsize)
        emit $ "call " ++ label
        emitAdjustBase (- si - wordsize)

emitLet :: Environment -> StackIndex -> [Binding] -> Expr -> CodeGen
emitLet env si bindings body = emitLet' env si bindings
  where emitLet' env' si' [] = emitExpr env' si' body
        emitLet' env' si' (binding : bindingsTail) =
          do emitExpr env si' (expr binding)
             emitStackSave si'
             emitLet' (insertVarBinding (name binding) si' env') (nextStackIndex si') bindingsTail

emitLetStar :: Environment -> StackIndex -> [Binding] -> Expr -> CodeGen
emitLetStar env si bindings body = emitLetStar' env si bindings
  where emitLetStar' env' si' [] = emitExpr env' si' body
        emitLetStar' env' si' (binding : bindingsTail) =
          do emitExpr env' si' (expr binding) -- Only difference with `emitLet` is in this line (the first argument)
             emitStackSave si'
             emitLetStar' (insertVarBinding (name binding) si' env') (nextStackIndex si') bindingsTail

emitFnApp :: String -> Environment -> StackIndex -> FnGen -> [Expr] -> CodeGen
emitFnApp fnName env si fnGen args =
  case fnGen of
    UnaryFn codeGen -> do
      emitArgs env si args -- @TODO check # of args
      codeGen
    BinaryFn codeGen ->
      if elem fnName canBeOptimized
      then emitBinaryFnOpt codeGen args env si
      else emitBinaryFnApp codeGen args env si
    Predicate predicate -> do
      emitArgs env si args -- @TODO check # of args
      predicate
      ifEqReturnTrue
    Comparison compareType compareBody -> do
      emitArgs env si args -- @TODO check # of args
      compareBody (stackValueAt si)
      emitBooleanByComparison compareType

canBeOptimized :: [String]
canBeOptimized = [ "fx+"
                 ]

emitBinaryFnApp :: (Register -> Register -> CodeGen) -> [Expr] -> Environment -> StackIndex -> CodeGen
emitBinaryFnApp codeGen args env si = do
    emitArgs env si args
    codeGen (stackValueAt si) eax

emitBinaryFnOpt :: (Register -> Register -> CodeGen) -> [Expr] -> Environment -> StackIndex -> CodeGen
emitBinaryFnOpt codeGen args env si = case args of
  [L arg1, arg2] -> do
    emitExpr env si arg2
    codeGen ("$" ++ (show $ inmediateRepr arg1)) eax
  [arg1, L arg2] -> do
    emitExpr env si arg1
    codeGen ("$" ++ (show $ inmediateRepr arg2)) eax
  _              ->
    emitBinaryFnApp codeGen args env si

movl :: Register -> Register -> CodeGen
movl = binOp "movl"

emitStackLoad :: StackIndex -> CodeGen
emitStackLoad si  = movl (stackValueAt si) eax

emitStackSave :: StackIndex -> CodeGen
emitStackSave si = movl eax (stackValueAt si)

nextStackIndex :: StackIndex -> StackIndex
nextStackIndex si = si - wordsize

emitArgs :: Environment -> StackIndex -> [Expr] -> CodeGen
emitArgs env si args = loop si args noop
  where loop _   []              gen =
          gen
        loop si' (argh:argsTail) gen =
          let gen' = do gen
                        emitExpr env si' argh
                        emitStackSave si'
          in loop (nextStackIndex si') argsTail gen'

primitives :: Map String FnGen
primitives = unaryPrims `union` binaryPrims

unaryPrims :: Map String FnGen
unaryPrims = fromList [ ("fxadd1"      , fxadd1)
                      , ("fxsub1"      , fxsub1)
                      , ("char->fixnum", charToFixNum)
                      , ("fixnum->char", fixNumToChar)
                      , ("fxlognot"    , fxLogNot)
                      , ("fixnum?"     , isFixnum)
                      , ("null?"       , isNull)
                      , ("not"         , notL)
                      , ("boolean?"    , isBoolean)
                      , ("char?"       , isChar)
                      , ("fxzero?"     , isFxZero)
                      ]

binaryPrims :: Map String FnGen
binaryPrims = fromList [ ("fx+"      , fxPlus)
                       , ("fx-"      , fxMinus)
                       , ("fx*"      , fxTimes)
                       , ("fxlogand" , fxLogAnd)
                       , ("fxlogor"  , fxLogOr)
                       , ("fx="      , fxEq)
                       , ("fx<"      , fxLess)
                       , ("fx<="     , fxLessOrEq)
                       , ("fx>"      , fxGreater)
                       , ("fx>="     , fxGreaterOrEq)
                       ]

fxadd1 = UnaryFn $
  emit $ "addl $" ++ (show $ inmediateRepr $ FixNum 1) ++ ", %eax"

fxsub1 :: FnGen
fxadd1 :: FnGen
fxsub1 = UnaryFn $
  emit $ "subl $" ++ (show $ inmediateRepr $ FixNum 1) ++ ", %eax"

charToFixNum :: FnGen
charToFixNum = UnaryFn $
  emit $ "sarl $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the right 6 bits (remember char tag is 00001111)

fixNumToChar :: FnGen
fixNumToChar = UnaryFn $ do
  emit $ "sall $" ++ show (charShift - intShift)  ++ ", %eax" -- move to the left 6 bits
  emit $ "orl $" ++ show charTag ++ ", %eax" -- add char tag

compareTo :: Integer -> CodeGen
compareTo n =
  emit $ "cmp $" ++ show n ++ ", %al"

data ComparisonType = Eq
                    | Less
                    | LessOrEq
                    | Greater
                    | GreaterOrEq
                    | NotEq

opposing :: ComparisonType -> ComparisonType
opposing Eq          = NotEq
opposing Less        = GreaterOrEq
opposing LessOrEq    = Greater
opposing Greater     = LessOrEq
opposing GreaterOrEq = Less
opposing NotEq       = Eq

type Label = String

comparisonToJump :: ComparisonType -> String
comparisonToJump Eq          = "je"
comparisonToJump Less        = "jl"
comparisonToJump LessOrEq    = "jle"
comparisonToJump Greater     = "jg"
comparisonToJump GreaterOrEq = "jge"
comparisonToJump NotEq       = "jne"

comparisonToSet :: ComparisonType -> String
comparisonToSet Eq          = "sete"
comparisonToSet Less        = "setl"
comparisonToSet LessOrEq    = "setle"
comparisonToSet Greater     = "setg"
comparisonToSet GreaterOrEq = "setge"
comparisonToSet NotEq       = "setne"

ifComparisonJumpTo :: ComparisonType -> Label -> CodeGen
ifComparisonJumpTo compareType label =
  emit $ comparisonToJump compareType ++" " ++ label

ifEqJumpTo :: Label -> CodeGen
ifEqJumpTo = ifComparisonJumpTo Eq

ifNotEqJumpTo :: Label -> CodeGen
ifNotEqJumpTo = ifComparisonJumpTo NotEq

jumpTo :: Label -> CodeGen
jumpTo label =
  emit $ "jmp " ++ label

emitLabel :: Label -> CodeGen
emitLabel label =
  emitNoTab $ label ++ ":"

isNull :: FnGen
isNull = Predicate $ compareTo nilValue

isBoolean :: FnGen
isBoolean = Predicate $ do
  applyMask boolMask
  compareTo falseValue

isChar :: FnGen
isChar = Predicate $ do
    applyMask charMask
    compareTo $ toInteger charTag

isFixnum :: FnGen
isFixnum = Predicate $ do
  applyMask $ toInteger intTag
  compareTo 0

notL :: FnGen
notL = Predicate $ compareTo falseValue

isFxZero :: FnGen
isFxZero = Predicate $ compareTo 0

fxLogNot :: FnGen
fxLogNot = UnaryFn $ do
  emit "not %eax"
  emit "sar $2, %eax"
  emit "sal $2, %eax"

uniqueLabel :: GenState Label
uniqueLabel = do
  s <- get
  modify (+1)
  return $ "L_" ++ show s

uniqueLambdaLabel :: String -> GenState Label
uniqueLambdaLabel fnName = do
  s <- get
  modify (+1)
  return $ "Lambda_" ++ show s

emitIf :: Environment -> StackIndex -> Expr -> Expr -> Expr -> CodeGen
emitIf env si condition conseq altern = do
  alternLabel <- uniqueLabel
  endLabel    <- uniqueLabel
  let emitIfFor :: CodeGen -> CodeGen
      emitIfFor code = do
        code
        compareTo falseValue
        ifEqJumpTo alternLabel
  let evalCondAndJumpToAlternIfFalse =
        case condition of
          FnApp fnName args ->
            let (Just primitive) = lookup fnName primitives  -- @TODO handle this
                emitAndJump (UnaryFn fnCode) =
                  emitIfFor fnCode
                emitAndJump (BinaryFn fnCode) =
                  emitIfFor $ fnCode (stackValueAt si) eax
                emitAndJump (Predicate predicateCode) = do
                  predicateCode
                  ifNotEqJumpTo alternLabel
                emitAndJump (Comparison comparisonType fnCode)  = do
                  fnCode (stackValueAt si)
                  ifComparisonJumpTo (opposing comparisonType) alternLabel
            in do emitArgs env si args
                  emitAndJump primitive
          _ -> do emitExpr env si condition
                  compareTo falseValue
                  ifEqJumpTo alternLabel
  evalCondAndJumpToAlternIfFalse
  emitExpr env si conseq
  jumpTo endLabel
  emitLabel alternLabel
  emitExpr env si altern
  emitLabel endLabel

emitAnd :: Environment -> StackIndex -> [Expr] -> CodeGen
emitAnd env si []            = emitExpr env si _False
emitAnd env si [test]        = emitExpr env si test
emitAnd env si (test : rest) = emitIf env si test (And rest) _False

emitOr :: Environment -> StackIndex -> [Expr] -> CodeGen
emitOr env si []            = emitExpr env si _True
emitOr env si [test]        = emitExpr env si test
emitOr env si (test : rest) = emitIf env si test NoOp (Or rest)

type StackIndex = Integer

stackValueAt :: StackIndex -> String
stackValueAt si = show si ++ "(%esp)"

type Inst = String
type Op = String
type Register = String

binOp :: Op -> Register -> Register -> CodeGen
binOp op reg1 reg2 =
  emit $ op ++ " " ++ reg1 ++ ", " ++ reg2

eax :: String
eax = "%eax"

addl :: Register -> Register -> CodeGen
addl = binOp "addl"

fxPlus :: FnGen
fxPlus = BinaryFn addl

mov :: Register -> Register -> CodeGen
mov reg1 reg2 = binOp "mov" reg1 reg2

subl :: Register -> Register -> CodeGen
subl = binOp "subl"

fxMinus :: FnGen
fxMinus = BinaryFn $ \reg1 reg2 -> do
  subl reg2 reg1
  mov  reg1 reg2

shr :: Int -> Register -> CodeGen
shr n reg2 = binOp "shr" ("$" ++ show n) reg2

-- normally when we multiply two numbers they are shifted 2 bits
-- to the right. That's: a number x is represented by 4*x
-- So if we want to get the product of x and y we can produce
-- the number 4*x*y. This can be done by shifting one of the numbers
-- two bits to the left and then multiplying it by the other.
-- (4x) * y
-- 4(x*y)
fxTimes :: FnGen
fxTimes = BinaryFn $ \reg1 reg2 -> do
  shr intShift reg2
  emit $ "mull " ++ reg1

_and :: Register -> Register -> CodeGen
_and = binOp "and"

fxLogAnd :: FnGen
fxLogAnd = BinaryFn _and

_or :: Register -> Register -> CodeGen
_or = binOp "or"

fxLogOr :: FnGen
fxLogOr = BinaryFn _or

cmp :: Register -> Register -> CodeGen
cmp = binOp "cmp"

compareEaxToStackValue :: Register -> CodeGen
compareEaxToStackValue si = cmp "%eax" si

fxComparison :: ComparisonType -> FnGen
fxComparison comparisonType =
  Comparison comparisonType compareEaxToStackValue

fxEq :: FnGen
fxEq = fxComparison Eq

fxLess :: FnGen
fxLess = fxComparison Less

fxLessOrEq :: FnGen
fxLessOrEq = fxComparison LessOrEq

fxGreater :: FnGen
fxGreater = fxComparison Greater

fxGreaterOrEq :: FnGen
fxGreaterOrEq = fxComparison GreaterOrEq

insertFnBinding :: FnName -> Label -> Environment -> Environment
insertFnBinding fnName label env =
  env { fnEnv = insert fnName label (fnEnv env) }

emitLambda :: Label -> Environment -> StackIndex -> Lambda -> CodeGen
emitLambda label env si lmb = do
  emitLabel label
  emitParams env si (params lmb)
  where emitParams env' si' [] = do
          emitExpr env' si' (body lmb)
          emit "ret"
        emitParams env' si' (arg:restOfArgs) =
          emitParams (insertVarBinding arg si' env') (nextStackIndex si') restOfArgs

emitLetRec :: [LambdaBinding] -> Expr -> CodeGen
emitLetRec bindings body = do
  env <- foldM emitLambdaBinding initialEnvironment bindings
  wrapInEntryPoint $ emitExpr env si body
  where si = initialStackIndex
        emitLambdaBinding env (LambdaBinding fnName lambda) =
          do label <- uniqueLambdaLabel fnName
             emitLambda label env si lambda
             return $ insertFnBinding fnName label env

emitProgram :: Program -> CodeGen
emitProgram (Expr expr) =
  wrapInEntryPoint $ emitExpr initialEnvironment initialStackIndex expr
emitProgram (LetRec bindings body) =
  emitLetRec bindings body

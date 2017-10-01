module Emitter where

import Prelude hiding (lookup)
import Control.Monad.Writer.Lazy
import MagicNumbers
import Expr
import InmediateRepr
import Control.Monad.State.Lazy
import Data.HashMap hiding (map)
import Control.Monad.Reader

type Code = [String]

-- The id for the function labels
type CodeGenState = Int

type IsTail = Bool

type GenReaderState = StateT CodeGenState (ReaderT (StackIndex, Environment, IsTail) (Writer Code))

type CodeGen = GenReaderState ()

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

initialIsTail :: IsTail
initialIsTail = True

compile :: Program -> Code
compile = executeGen . emitProgram

executeGen :: CodeGen -> Code
executeGen codeGen = execWriter $
  runReaderT (evalStateT codeGen initialState)
             (initialStackIndex, initialEnvironment, initialIsTail)

tabbed :: String -> String
tabbed s = "    " ++ s

emit :: String -> CodeGen
emit s = tell [ tabbed s ]

emitAll :: [String] -> CodeGen
emitAll = tell

emitNoTab :: String -> CodeGen
emitNoTab s = tell [s]

noop :: CodeGen
noop = tell []

emitFunctionHeader :: Label -> CodeGen
emitFunctionHeader label = do
  emitNoTab $ ".globl " ++ label
  emitNoTab $ ".type " ++ label ++ ", @function"
  emitLabel label

saveRegistersInsts :: [String]
saveRegistersInsts = map tabbed
  [ "movl 4(%esp), %ecx"  -- first parameter (4%esp) points to context struct
  , "movl %ebx, 4(%ecx)"
  , "movl %esi, 16(%ecx)"
  , "movl %edi, 20(%ecx)"
  , "movl %ebp, 24(%ecx)"
  , "movl %esp, 28(%ecx)"
  ]

loadHeapAndStackPointersInsts :: [String]
loadHeapAndStackPointersInsts = map tabbed
  [ "movl 12(%esp), %ebp" -- load heap  pointer from 3rd parameter
  , "movl 8(%esp) , %esp" -- load stack pointer from 2nd parameter
  ]

restoreRegistersInsts :: [String]
restoreRegistersInsts = map tabbed
  [ "movl 4(%ecx), %ebx"  -- restore C's registers
  , "movl 16(%ecx), %esi"
  , "movl 20(%ecx), %edi"
  , "movl 24(%ecx), %ebp"
  , "movl 28(%ecx), %esp"
  ]

wrapInEntryPoint :: CodeGen -> CodeGen
wrapInEntryPoint code = do
  emitNoTab ".text"
  emitFunctionHeader "L_scheme_entry"
  code
  emit "ret"
  emitFunctionHeader "scheme_entry"
  emitAll saveRegistersInsts
  emitAll loadHeapAndStackPointersInsts
  emit "call L_scheme_entry"
  emitAll restoreRegistersInsts
  emit "ret"

emitLiteral :: Integer -> CodeGen
emitLiteral n = do
  emit $ "movl $" ++ show n ++ ", %eax"

applyMask :: Integer -> CodeGen
applyMask mask =
  emit $ "and $" ++ show mask ++ ", %eax"

emitBooleanByComparison :: ComparisonType -> CodeGen
emitBooleanByComparison compareType = do
  emit $ comparisonToSet compareType ++ " %al"  -- set %al to the result of the comparison
  emit $ "movzbl %al, %eax"                   -- mov %al to %eax and pad the remaining bits with 0: https://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Move_with_zero_extend --> why is this needed?
  emit $ "sal $6, %al"                        -- move the result bit 6 bits to the left
  emit $ "or $" ++ show falseValue ++ ", %al" -- or with the false value to return a "boolean" in the expected format

ifEqReturnTrue :: CodeGen
ifEqReturnTrue = emitBooleanByComparison Eq

getEnv :: GenReaderState Environment
getEnv = fmap (\(_,env,_) -> env) ask

getStackIndex :: GenReaderState StackIndex
getStackIndex = fmap (\(si,_,_) -> si) ask

isInTailPosition :: GenReaderState IsTail
isInTailPosition = fmap (\(_,_,isTail) -> isTail) ask

getVarIndex :: VarName -> GenReaderState StackIndex
getVarIndex varName = do
  env <- getEnv
  let Just si = lookup varName (varEnv env) -- @TODO handle this
  return si

ret :: CodeGen
ret = emit "ret"

emitReturnIfTail :: CodeGen
emitReturnIfTail = do
  isTail <- isInTailPosition
  if isTail
  then ret
  else noop

emitExprBase :: Expr -> CodeGen
emitExprBase (L literal) = do
  emitLiteral $ inmediateRepr literal
  emitReturnIfTail
emitExprBase (PrimitiveApp name args) =
  let (Just primitive) = lookup name primitives  -- @TODO handle this
  in do emitPrimitiveApp name primitive args
        emitReturnIfTail
emitExprBase (If condition conseq altern) =
  emitIf condition conseq altern
emitExprBase (And preds) =
  emitAnd preds
emitExprBase (Or preds) =
  emitOr preds
emitExprBase (Let bindings body) =
  emitLet bindings body
emitExprBase (LetStar bindings body) =
  emitLetStar bindings body
emitExprBase (VarRef varName) = do
  si <- getVarIndex varName
  emitStackLoad si
  emitReturnIfTail
emitExprBase (UserFnApp fnName args) =
  emitUserFnApp fnName args
emitExprBase NoOp =
  noop

withIsTailSetTo :: Bool -> GenReaderState a -> GenReaderState a
withIsTailSetTo isTail = local (\(env,si,_) -> (env,si,isTail))

emitExpr :: Expr -> CodeGen
emitExpr expr =
  withIsTailSetTo False (emitExprBase expr)

emitTailExpr :: Expr -> CodeGen
emitTailExpr expr =
  withIsTailSetTo True (emitExprBase expr)

insertVarBinding :: VarName -> StackIndex -> Environment -> Environment
insertVarBinding varName si env =
  env { varEnv = insert varName si (varEnv env) }

emitAdjustBase :: (StackIndex -> Integer) -> CodeGen
emitAdjustBase f = do
  si <- getStackIndex
  emit $ "addl $" ++ show (f si) ++ ", %esp"

getFnLabel :: FunctionName -> GenReaderState Label
getFnLabel fnName = do
  env <- getEnv
  let Just label = lookup fnName (fnEnv env) -- @TODO handle this
  return label

withNextIndex :: GenReaderState a -> GenReaderState a
withNextIndex =
  local (\(si,env,isTail) -> (nextStackIndex si, env, isTail))

collapseStack :: Int -> StackIndex -> StackIndex -> CodeGen
collapseStack 0 _ _ =
  noop
collapseStack n originalStackIndex argsStackIndex =
  do movl (stackValueAt argsStackIndex) eax
     movl eax (stackValueAt originalStackIndex)
     collapseStack (n - 1) (nextStackIndex originalStackIndex) (nextStackIndex argsStackIndex)

emitUserFnApp :: FunctionName -> [Expr] -> CodeGen
emitUserFnApp fnName args = do
  label <- getFnLabel fnName
  withNextIndex (emitArgs args)
  isTail <- isInTailPosition
  if isTail
    then do si <- getStackIndex
            collapseStack (length args) (- wordsize) (nextStackIndex si)
            emit $ "jmp " ++ label
    else do emitAdjustBase (\si -> si + wordsize)
            emit $ "call " ++ label
            emitAdjustBase (\si -> - si - wordsize)

withEnv :: Environment -> GenReaderState a -> GenReaderState a
withEnv env =
  local (\(si,_,isTail) -> (si,env,isTail))

-- @TODO refactor repetition between these 2 functions
emitLet :: [Binding] -> Expr -> CodeGen
emitLet bindings body = do
  env <- getEnv
  emitLet' bindings env
  where emitLet' [] env =
          withEnv env $ emitExprBase body
        emitLet' (binding : bindingsTail) env =
          do emitExpr (expr binding)
             emitStackSave
             si <- getStackIndex
             let nextEnv = insertVarBinding (name binding) si env
             withNextIndex $ emitLet' bindingsTail nextEnv

emitLetStar :: [Binding] -> Expr -> CodeGen
emitLetStar bindings body = emitLetStar' bindings
  where emitLetStar' [] =
          emitExprBase body
        emitLetStar' (binding : bindingsTail) =
          do emitExpr (expr binding)
             emitStackSave
             let withLocalSiEnv = local (\(si,env,isTail) -> (nextStackIndex si, insertVarBinding (name binding) si env, isTail) )
             withLocalSiEnv $ emitLetStar' bindingsTail

emitPrimitiveApp :: FunctionName -> FnGen -> [Expr] -> CodeGen
emitPrimitiveApp fnName fnGen args =
  case fnGen of
    UnaryFn codeGen -> do
      emitExpr $ head args -- @TODO check # of args
      codeGen
    BinaryFn codeGen ->
      if elem fnName canBeOptimized
      then emitBinaryFnOpt codeGen args
      else emitBinaryFnApp codeGen args
    Predicate predicate -> do
      emitArgs args -- @TODO check # of args
      predicate
      ifEqReturnTrue
    Comparison compareType compareBody -> do
      emitArgs args -- @TODO check # of args
      sv <- stackValueAtIndex
      compareBody sv
      emitBooleanByComparison compareType

canBeOptimized :: [String]
canBeOptimized = [ "fx+"
                 ]

emitBinaryFnApp :: (Register -> Register -> CodeGen) -> [Expr] -> CodeGen
emitBinaryFnApp codeGen args = do
    emitArgsWithoutLastSave args
    sv <- stackValueAtIndex
    codeGen sv eax

-- @TODO: this assumes commutativity?
emitBinaryFnOpt :: (Register -> Register -> CodeGen) -> [Expr] -> CodeGen
emitBinaryFnOpt codeGen args = case args of
  [L arg1, arg2] -> do
    emitExpr arg2
    codeGen ("$" ++ (show $ inmediateRepr arg1)) eax
  [arg1, L arg2] -> do
    emitExpr arg1
    codeGen ("$" ++ (show $ inmediateRepr arg2)) eax
  [VarRef arg1, VarRef arg2] -> do
    si1 <- getVarIndex arg1
    si2 <- getVarIndex arg2
    emitStackLoad si1
    codeGen (stackValueAt si2) eax
  [VarRef arg1, arg2] -> do
    emitExpr arg2
    si <- getVarIndex arg1
    codeGen (stackValueAt si) eax
  [arg1, VarRef arg2] -> do
    emitExpr arg1
    si <- getVarIndex arg2
    codeGen (stackValueAt si) eax
  _              ->
    emitBinaryFnApp codeGen args

movl :: Register -> Register -> CodeGen
movl = binOp "movl"

emitStackLoad :: StackIndex -> CodeGen
emitStackLoad si  = movl (stackValueAt si) eax

emitStackSave :: CodeGen
emitStackSave = do
  sv <- stackValueAtIndex
  movl eax sv

nextStackIndex :: StackIndex -> StackIndex
nextStackIndex si = si - wordsize

emitArgs :: [Expr] -> CodeGen
emitArgs args = loop args
  where loop []              =
          noop
        loop (argh:argsTail) = do
          emitExpr argh
          emitStackSave
          withNextIndex (loop argsTail)

emitArgsWithoutLastSave :: [Expr] -> CodeGen
emitArgsWithoutLastSave args = loop args
  where loop [] =
          noop
        loop [last] =
          emitExpr last
        loop (argh:argsTail) = do
          emitExpr argh
          emitStackSave
          withNextIndex (loop argsTail)

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
  emit $ "cmp $" ++ show n ++ ", %eax"

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

emitIfNotTail :: CodeGen -> CodeGen
emitIfNotTail codeGen = do
  isTail <- isInTailPosition
  if not isTail
    then codeGen
    else noop

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

uniqueLabel :: GenReaderState Label
uniqueLabel = do
  s <- get
  modify (+1)
  return $ "L_" ++ show s

uniqueLambdaLabel :: String -> GenReaderState Label
uniqueLambdaLabel fnName = do
  s <- get
  modify (+1)
  return $ "Lambda_" ++ show s

emitIf :: Expr -> Expr -> Expr -> CodeGen
emitIf condition conseq altern = do
  alternLabel <- uniqueLabel
  endLabel    <- uniqueLabel
  let emitIfFor :: CodeGen -> CodeGen
      emitIfFor code = do
        code
        compareTo falseValue
        ifEqJumpTo alternLabel
  let evalCondAndJumpToAlternIfFalse =
        case condition of
          PrimitiveApp fnName args ->
            let (Just primitive) = lookup fnName primitives  -- @TODO handle this
                emitAndJump (UnaryFn fnCode) =
                  emitIfFor fnCode
                emitAndJump (BinaryFn fnCode) = do
                  sv <- stackValueAtIndex
                  emitIfFor $ fnCode sv eax
                emitAndJump (Predicate predicateCode) = do
                  predicateCode
                  ifNotEqJumpTo alternLabel
                emitAndJump (Comparison comparisonType fnCode)  = do
                  sv <- stackValueAtIndex
                  fnCode sv
                  ifComparisonJumpTo (opposing comparisonType) alternLabel
            in do emitArgs args
                  emitAndJump primitive
          _ -> do emitExpr condition
                  compareTo falseValue
                  ifEqJumpTo alternLabel
  evalCondAndJumpToAlternIfFalse
  emitExprBase conseq
  emitIfNotTail $ jumpTo endLabel
  emitLabel alternLabel
  emitExprBase altern
  emitIfNotTail $ emitLabel endLabel

emitAnd :: [Expr] -> CodeGen
emitAnd []            = emitExpr _False
emitAnd [test]        = emitExpr test
emitAnd (test : rest) = emitIf test (And rest) _False

emitOr :: [Expr] -> CodeGen
emitOr []            = emitExpr _True
emitOr [test]        = emitExpr test
emitOr (test : rest) = emitIf test NoOp (Or rest)

type StackIndex = Integer

stackValueAt :: StackIndex -> String
stackValueAt si = (show si) ++ "(%esp)"

stackValueAtIndex :: GenReaderState String
stackValueAtIndex = do
  si <- getStackIndex
  return $ stackValueAt si

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

emitLambda :: Label -> Lambda -> CodeGen
emitLambda label lmb = do
  emitLabel label
  emitParams (params lmb)
  where emitParams [] = do
          emitTailExpr (body lmb)
        emitParams (arg:restOfArgs) = do
          let withLocalSiEnv = local (\(si,env,isTail) -> (nextStackIndex si, insertVarBinding arg si env, isTail) )
          withLocalSiEnv $ emitParams restOfArgs

-- @TODO whole function is very ugly, refactor!
emitLetRec :: [LambdaBinding] -> Expr -> CodeGen
emitLetRec bindings body = do
  lambdasWithLabels <- sequence $ map lambdaNameLabelPair bindings
  let fnBindings = fromList $ map (\(binding, label) -> (functionName binding, label)) lambdasWithLabels
  (si, emptyEnv, isTail) <- ask
  let envWithFnBindings = emptyEnv { fnEnv = fnBindings }
  let withFnBindings = local (const (si, envWithFnBindings, isTail))
  withFnBindings $ mapM_ emitLambdaBinding lambdasWithLabels
  wrapInEntryPoint $ withFnBindings (emitExpr body)
  where lambdaNameLabelPair binding =
          do label <- uniqueLambdaLabel (functionName binding)
             return (binding, label)
        emitLambdaBinding (LambdaBinding _ lambda, label) =
          emitLambda label lambda

emitProgram :: Program -> CodeGen
emitProgram (Expr expr) =
  wrapInEntryPoint $ emitExpr expr
emitProgram (LetRec bindings body) =
  emitLetRec bindings body

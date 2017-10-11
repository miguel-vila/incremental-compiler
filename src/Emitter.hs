module Emitter where

import Prelude hiding (lookup)
import Control.Monad.Writer.Lazy
import MagicNumbers
import Expr
import InmediateRepr
import Control.Monad.State.Lazy
import Data.HashMap hiding (map)
import Control.Monad.Reader
import CodeGen

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

saveRegistersInsts :: Code
saveRegistersInsts = map tabbed
  [ "movl 4(%esp), %ecx"  -- first parameter (4%esp) points to context struct
  , "movl %ebx, 4(%ecx)"
  , "movl %esi, 16(%ecx)"
  , "movl %edi, 20(%ecx)"
  , "movl %ebp, 24(%ecx)"
  , "movl %esp, 28(%ecx)"
  ]

loadHeapAndStackPointersInsts :: Code
loadHeapAndStackPointersInsts = map tabbed
  [ "movl 12(%esp), %ebp" -- load heap  pointer from 3rd parameter
  , "movl 8(%esp) , %esp" -- load stack pointer from 2nd parameter
  ]

restoreRegistersInsts :: Code
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

getVarIndex :: VarName -> GenReaderState StackIndex
getVarIndex varName = do
  env <- getEnv
  let Just si = lookup varName (varEnv env) -- @TODO handle this
  return si

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
emitExprBase (Do exprs) =
  mapM_ emitExprBase exprs -- last expression should leave it's result at %eax
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
    NaryFn codeGen -> do
      emitArgsWithoutLastSave args
      si <- getStackIndex
      codeGen si
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
emitBinaryFnApp codeGen [arg1, arg2] = do
    emitExpr arg1
    emitStackSave
    sv <- stackValueAtIndex
    withNextIndex $ emitExpr arg2
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
  where loop []              =
          noop
        loop [arg]           =
          emitExpr arg
        loop (argh:argsTail) = do
          emitExpr argh
          emitStackSave
          withNextIndex (loop argsTail)

primitives :: Map FunctionName FnGen
primitives = unaryPrims `union`
  binaryPrims `union`
  naryPrims

unaryPrims :: Map FunctionName FnGen
unaryPrims = fromList [ ("fxadd1"        , fxadd1)
                      , ("fxsub1"        , fxsub1)
                      , ("char->fixnum"  , charToFixNum)
                      , ("fixnum->char"  , fixNumToChar)
                      , ("fxlognot"      , fxLogNot)
                      , ("fixnum?"       , isFixnum)
                      , ("null?"         , isNull)
                      , ("not"           , notL)
                      , ("boolean?"      , isBoolean)
                      , ("char?"         , isChar)
                      , ("fxzero?"       , isFxZero)
                      , ("pair?"         , isPair)
                      , ("car"           , car)
                      , ("cdr"           , cdr)
                      , ("vector?"       , isVector)
                      , ("vector-length" , vectorLength)
                      ]

binaryPrims :: Map FunctionName FnGen
binaryPrims = fromList [ ("fx+"         , fxPlus)
                       , ("fx-"         , fxMinus)
                       , ("fx*"         , fxTimes)
                       , ("fxlogand"    , fxLogAnd)
                       , ("fxlogor"     , fxLogOr)
                       , ("fx="         , fxEq)
                       , ("fx<"         , fxLess)
                       , ("fx<="        , fxLessOrEq)
                       , ("fx>"         , fxGreater)
                       , ("fx>="        , fxGreaterOrEq)
                       , ("cons"        , cons)
                       , ("make-vector" , makeVector)
                       , ("vector-ref"  , vectorRef)
                       ]

naryPrims :: Map FunctionName FnGen
naryPrims = fromList [ ("vector-set!" , vectorSet)
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

ifComparisonJumpTo :: ComparisonType -> Label -> CodeGen
ifComparisonJumpTo compareType label =
  emit $ comparisonToJump compareType ++" " ++ label

ifEqJumpTo :: Label -> CodeGen
ifEqJumpTo = ifComparisonJumpTo Eq

ifNotEqJumpTo :: Label -> CodeGen
ifNotEqJumpTo = ifComparisonJumpTo NotEq

emitIfNotTail :: CodeGen -> CodeGen
emitIfNotTail codeGen = do
  isTail <- isInTailPosition
  if not isTail
    then codeGen
    else noop

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

isPair :: FnGen
isPair = Predicate $ do
  applyMask pairMask
  compareTo (toInteger pairTag)

isVector :: FnGen
isVector = Predicate $ do
  applyMask vectorMask
  compareTo (toInteger vectorTag)

vectorLength :: FnGen
vectorLength = UnaryFn $ do
  emit $ "sub $" ++ show vectorTag ++ ", %eax"
  mov "-4(%eax)" eax

fxLogNot :: FnGen
fxLogNot = UnaryFn $ do
  emit "not %eax"
  emit "sar $2, %eax"
  emit "sal $2, %eax"

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

fxPlus :: FnGen
fxPlus = BinaryFn addl

fxMinus :: FnGen
fxMinus = BinaryFn $ \reg1 reg2 -> do
  subl reg2 reg1
  mov  reg1 reg2

fxLogAnd :: FnGen
fxLogAnd = BinaryFn _and

fxLogOr :: FnGen
fxLogOr = BinaryFn _or

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

heapPosWithOffset :: Integer -> String
heapPosWithOffset offset =
  show offset ++ "(%ebp)"

cons :: FnGen
cons = BinaryFn _cons
  where _cons reg1 eax = do
          mov eax (heapPosWithOffset cdrOffset)
          mov reg1 eax
          mov eax (heapPosWithOffset carOffset)
          mov "%ebp" eax
          emit $ "orl $" ++ show pairTag ++ ", %eax"
          subl "$8" "%ebp"

makeVector :: FnGen
makeVector = BinaryFn mkVector
  where mkVector n eax = do
          loopLabel <- uniqueLabel
          mov n "%ebx"
          mov "%ebx" (heapPosWithOffset (-4)) -- save length
          mov "%ebp" "%ebx" -- %ebx will be iterator
          emit $ "sub $" ++ show vectorLengthOffset ++", %ebx" -- move it down after saving the length
          mov "%ebp" "%edx" -- %edx will be the limit
          emit $ "sub " ++ n ++ ", %edx" -- for each of the elements (4x)
          emit $ "sub $4, %edx" -- for the length
          emitLabel loopLabel
          emit $ "sub $4, %ebx"
          mov eax "(%ebx)" -- save the element in the i-th entry
          emit $ "cmp %edx, %ebx"
          emit $ "jne " ++ loopLabel
          mov "%ebp" eax
          emit $ "or $" ++ show vectorTag ++ ", %eax"
          mov "%ebx" "%ebp"

car :: FnGen
car = UnaryFn $ do
  emit $ "subl $" ++ show pairTag ++ ", %eax"
  mov (show carOffset ++ "(%eax)") eax

cdr :: FnGen
cdr = UnaryFn $ do
  emit $ "subl $" ++ show pairTag ++ ", %eax"
  mov (show cdrOffset ++ "(%eax)") eax

vectorRef :: FnGen
vectorRef = BinaryFn vctRef
  where vctRef vec pos = do
          mov vec "%ebx"
          emit $ "sub $" ++ show vectorTag ++ ", %ebx"
          emit $ "sub $" ++ show vectorContentOffset ++ ", %ebx"
          emit $ "sub " ++ pos ++ ", %ebx"
          mov "(%ebx)" eax

vectorSet :: FnGen
vectorSet = NaryFn $ \si ->
  let vec = stackValueAt si                  -- 1st arg
      pos = stackValueAt (nextStackIndex si) -- 2nd arg
      val = "%eax"                           -- 3rd arg
  in do
    mov vec "%ebx"
    emit $ "sub $" ++ show vectorTag ++ ", %ebx"
    emit $ "sub $" ++ show vectorContentOffset ++ ", %ebx"
    emit $ "sub " ++ pos ++ ", %ebx"
    mov val "(%ebx)"

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

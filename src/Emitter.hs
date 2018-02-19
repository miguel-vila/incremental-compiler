module Emitter where

import Prelude hiding (lookup)
import Data.HashMap hiding (map)
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Except

import MagicNumbers
import Expr hiding (_and, _or)
import InmediateRepr
import CodeGen
import Desugaring
import Data.Functor.Foldable hiding (Nil)
import Primitives

type Compiled = (Expr, CodeGen)

initialState :: CodeGenState
initialState = 0

initialStackIndex :: StackIndex
initialStackIndex = - wordsize

initialEnvironment :: Environment
initialEnvironment = Environment empty empty

initialIsTail :: IsTail
initialIsTail = True

executeGen :: CodeGen -> CompilerContext -> Except CompilationError Code
executeGen codeGen initialContext = execWriterT $
  runReaderT (evalStateT codeGen initialState) initialContext

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

ifEqReturnTrue :: CodeGen
ifEqReturnTrue = emitBooleanByComparison Eq

getVarIndex :: VarName -> GenReaderState StackIndex
getVarIndex varName = do
  env <- getEnv
  maybe (throwError (VariableNotInScope varName)) return (lookup varName (varEnv env))

emitReturnIfTail :: CodeGen
emitReturnIfTail = do
  isTail <- isInTailPosition
  if isTail
  then emit "ret"
  else noop

emitExprBase :: Expr -> CodeGen
emitExprBase = para ralg where
  ralg :: ExprF Compiled -> CodeGen
  ralg (L lit) = do
    emitLiteral (inmediateRepr lit)
    emitReturnIfTail
  ralg (PrimitiveApp name args) =
    do primitive <- lookupPrimitive name
       emitPrimitiveApp name primitive args
       emitReturnIfTail
  ralg (If condition conseq altern) =
    emitIf condition conseq altern
  ralg (Let bindings letBody) =
    emitLet bindings letBody
  ralg (LetStar bindings letBody) =
    emitLetStar bindings letBody
  ralg (VarRef varName) = do
    si <- getVarIndex varName
    emitStackLoad si
    emitReturnIfTail
  ralg (UserFnApp fnName args) =
    emitUserFnApp fnName args
  ralg (Do exprs) =
    mapM_ snd exprs -- last expression should leave it's result at %eax
  ralg NoOp =
    noop

withIsTailSetTo :: Bool -> GenReaderState a -> GenReaderState a
withIsTailSetTo isTail = local (\(env,si,_) -> (env,si,isTail))

asExpr :: CodeGen -> CodeGen
asExpr = withIsTailSetTo False

asTailExpr :: CodeGen -> CodeGen
asTailExpr = withIsTailSetTo True

insertVarBinding :: VarName -> StackIndex -> Environment -> Environment
insertVarBinding varName si env =
  env { varEnv = insert varName si (varEnv env) }

emitAdjustBase :: (StackIndex -> Integer) -> CodeGen
emitAdjustBase f = do
  si <- getStackIndex
  addl ("$" ++ show (f si)) "%esp"

getFnLabel :: FunctionName -> GenReaderState Label
getFnLabel fnName = do
  env <- getEnv
  maybe (throwError (FunctionNotDefined fnName)) return (lookup fnName (fnEnv env))

collapseStack :: Int -> StackIndex -> StackIndex -> CodeGen
collapseStack 0 _ _ =
  noop
collapseStack n originalStackIndex argsStackIndex =
  do movl (stackValueAt argsStackIndex) "%eax"
     movl "%eax" (stackValueAt originalStackIndex)
     collapseStack (n - 1) (nextStackIndex originalStackIndex) (nextStackIndex argsStackIndex)

emitUserFnApp :: FunctionName -> [Compiled] -> CodeGen
emitUserFnApp fnName args = do
  label <- getFnLabel fnName
  withNextIndex (emitArgs $ map snd args)
  isTail <- isInTailPosition
  if isTail
    then do si <- getStackIndex
            collapseStack (length args) (- wordsize) (nextStackIndex si)
            emit $ "jmp " ++ label
    else do emitAdjustBase (+ wordsize)
            emit $ "call " ++ label
            emitAdjustBase (\si -> - si - wordsize)

-- @TODO refactor repetition between these 2 functions
emitLet :: [BindingF Compiled] -> Compiled -> CodeGen
emitLet bindings letBody = do
  env <- getEnv
  emitLet' bindings env
  where emitLet' [] env =
          withEnv env (snd letBody)
        emitLet' ((bindingName, (_,bindingExpr)) : bindingsTail) env =
          do asExpr bindingExpr
             emitStackSave
             si <- getStackIndex
             let nextEnv = insertVarBinding bindingName si env
             withNextIndex $ emitLet' bindingsTail nextEnv

emitLetStar :: [BindingF Compiled] -> Compiled -> CodeGen
emitLetStar bindings letBody = emitLetStar' bindings
  where emitLetStar' [] =
          snd letBody
        emitLetStar' ((bindingName, (_,bindingExpr)) : bindingsTail) =
          do asExpr bindingExpr
             emitStackSave
             let withLocalSiEnv = local (\(si,env,isTail) -> (nextStackIndex si, insertVarBinding bindingName si env, isTail) )
             withLocalSiEnv $ emitLetStar' bindingsTail

emitPrimitiveApp :: FunctionName -> FnGen -> [Compiled] -> CodeGen
emitPrimitiveApp fnName fnGen args =
  case fnGen of
    UnaryFn codeGen -> do
      asExpr $ snd $ head args -- @TODO check # of args
      codeGen
    BinaryFn codeGen ->
      if fnName `elem` canBeOptimized
      then emitBinaryFnOpt codeGen args
      else emitBinaryFnApp codeGen args
    NaryFn codeGen -> do
      emitArgsWithoutLastSave $ map snd args
      si <- getStackIndex
      codeGen si
    Predicate predicate -> do
      emitArgs $ map snd args -- @TODO check # of args
      predicate
      ifEqReturnTrue
    Comparison compareType compareBody -> do
      emitArgs $ map snd args -- @TODO check # of args
      sv <- stackValueAtIndex
      compareBody sv
      emitBooleanByComparison compareType

canBeOptimized :: [String]
canBeOptimized = [ "fx+"
                 ]
emitBinaryFnApp :: (Register -> Register -> CodeGen) -> [Compiled] -> CodeGen
emitBinaryFnApp codeGen [arg1, arg2] = do
    asExpr $ snd arg1
    emitStackSave
    sv <- stackValueAtIndex
    withNextIndex $ asExpr $ snd arg2
    codeGen sv "%eax"

-- @TODO: this assumes commutativity?
emitBinaryFnOpt :: (Register -> Register -> CodeGen) -> [Compiled] -> CodeGen
emitBinaryFnOpt codeGen args = case args of
 [(Fix (L arg1),_), (_,arg2)] -> do
   asExpr arg2
   codeGen ("$" ++ show (inmediateRepr arg1)) "%eax"
 [(_,arg1), (Fix (L arg2),_)] -> do
   asExpr arg1
   codeGen ("$" ++ show (inmediateRepr arg2)) "%eax"
 [(Fix (VarRef arg1),_), (Fix (VarRef arg2),_)] -> do
   si1 <- getVarIndex arg1
   si2 <- getVarIndex arg2
   emitStackLoad si1
   codeGen (stackValueAt si2) "%eax"
 [(Fix (VarRef arg1),_), (_,arg2)] -> do
   asExpr arg2
   si <- getVarIndex arg1
   codeGen (stackValueAt si) "%eax"
 [(_,arg1), (Fix (VarRef arg2),_)] -> do
   asExpr arg1
   si <- getVarIndex arg2
   codeGen (stackValueAt si) "%eax"
 _              ->
   emitBinaryFnApp codeGen args

emitArgs :: [CodeGen] -> CodeGen
emitArgs = loop
  where loop []              =
          noop
        loop (argh:argsTail) = do
          asExpr argh
          emitStackSave
          withNextIndex (loop argsTail)

emitArgsWithoutLastSave :: [CodeGen] -> CodeGen
emitArgsWithoutLastSave = loop
  where loop []              =
          noop
        loop [arg]           =
          asExpr arg
        loop (argh:argsTail) = do
          asExpr argh
          emitStackSave
          withNextIndex (loop argsTail)

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

emitIf :: Compiled -> Compiled -> Compiled -> CodeGen
emitIf (conditionBody, condition) (_,conseq) (_,altern) = do
  alternLabel <- uniqueLabel
  endLabel    <- uniqueLabel
  let emitIfFor :: CodeGen -> CodeGen
      emitIfFor code = do
        code
        compareTo falseValue
        ifEqJumpTo alternLabel
  let emitArgs' []    = noop
      emitArgs' (argh: argsTail) = do
        asExpr $ emitExprBase argh -- @TODO <- argh, vanilla recursion
        emitStackSave
        withNextIndex (emitArgs' argsTail)
  let evalCondAndJumpToAlternIfFalse =
        case unfix conditionBody of
          PrimitiveApp fnName args ->
            let emitAndJump (UnaryFn fnCode) =
                  emitIfFor fnCode
                emitAndJump (BinaryFn fnCode) = do
                  sv <- stackValueAtIndex
                  emitIfFor $ fnCode sv "%eax"
                emitAndJump (Predicate predicateCode) = do
                  predicateCode
                  ifNotEqJumpTo alternLabel
                emitAndJump (Comparison comparisonType fnCode)  = do
                  sv <- stackValueAtIndex
                  fnCode sv
                  ifComparisonJumpTo (opposing comparisonType) alternLabel
            in do primitive <- lookupPrimitive fnName
                  emitArgs' args
                  emitAndJump primitive
          _ -> do asExpr condition
                  compareTo falseValue
                  ifEqJumpTo alternLabel
  evalCondAndJumpToAlternIfFalse
  conseq
  emitIfNotTail $ jumpTo endLabel
  emitLabel alternLabel
  altern
  emitIfNotTail $ emitLabel endLabel

emitLambda :: Label -> Lambda -> CodeGen
emitLambda label lmb = do
  emitLabel label
  emitParams (params lmb)
  where emitParams [] =
          asTailExpr $ emitExprBase (body lmb)
        emitParams (arg:restOfArgs) = do
          let withLocalSiEnv = local (\(si,env,isTail) -> (nextStackIndex si, insertVarBinding arg si env, isTail) )
          withLocalSiEnv $ emitParams restOfArgs

compile :: Program -> Except CompilationError Code
compile (Expr expr) =
  let gen = wrapInEntryPoint $ asExpr $ emitExprBase $ desugar expr
  in executeGen gen (initialStackIndex, initialEnvironment, initialIsTail)
compile (LetRec regularBindings programBody) =
  let bindings = (map (fmap desugar) regularBindings)
      lambdasWithLabels = zip bindings (uniqueLambdaLabels (length bindings))
      fnBindings = fromList $ map (\(bind, label) -> (functionName bind, label)) lambdasWithLabels
      envWithFnBindings = Environment { varEnv = empty, fnEnv = fnBindings }
      initial = (initialStackIndex, envWithFnBindings, initialIsTail)
      emitLambdaBinding (LambdaBinding _ lmb, label) = emitLambda label lmb
      gen = do mapM_ emitLambdaBinding lambdasWithLabels
               wrapInEntryPoint (asExpr $ emitExprBase (desugar programBody))
  in executeGen gen initial
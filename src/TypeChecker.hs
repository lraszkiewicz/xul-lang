{-# LANGUAGE ParallelListComp #-}

module TypeChecker where

import Control.Monad.RWS
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Debug.Trace

import AbsXul
import qualified Interpreter
import ParXul
import PrintXul

type FunEnv = Interpreter.FunEnv
-- TypeTraits represents information about the type and traits of a variable,
-- and TypeVarEnv stores that information for each variable.
-- InCurrentBlock says whether the variable was declared
-- in the current block or somewhere higher. When entering a block,
-- all values are set to HigherBlock (function enterBlock).
data InCurrentBlock
  = CurrentBlock
  | HigherBlock
-- ReadOnly is used for `for` loop counters, everything else is Writable.
data IsReadOnly
  = ReadOnly
  | Writable
type TypeTraits = (Type, InCurrentBlock, IsReadOnly)
type TypeVarEnv = Map Ident TypeTraits
-- The Type in (TypeVarEnv, Type) is the return type of the current function.
type TypeState = (TypeVarEnv, Type)
type TypeMonad = RWST FunEnv () TypeState IO

assert :: Print a => a -> Type -> Type -> TypeMonad Type
assert e t1 t2 = if t1 == t2
  then return t1
  else errorWithoutStackTrace $
    "Type " ++ show t1 ++ " does not match type " ++ show t2 ++ " in:\n"
    ++ printTree e

assertMany :: Print a => a -> [Type] -> Type -> TypeMonad Type
assertMany e ts t = if t `elem` ts
  then return t
  else errorWithoutStackTrace $
    "Type " ++ show t ++ " is not one of " ++ show ts ++ " in:\n" ++ printTree e

assertWritable :: Ident -> TypeMonad ()
assertWritable ident@(Ident name) = do
  (varEnv, _) <- get
  case Map.lookup ident varEnv of
    Just (_, _, Writable) -> return ()
    Just (_, _, ReadOnly) ->
      errorWithoutStackTrace $ "Variable `" ++ name ++ "` is read-only."
    _ -> return ()

getVarType :: Ident -> TypeMonad Type
getVarType ident@(Ident name) = do
  (varEnv, _) <- get
  if Map.member ident varEnv
    then let (varType, _, _) = varEnv ! ident in return varType
    else errorWithoutStackTrace $
      "Variable `" ++ name ++ "` was not declared in this scope."

enterBlock :: TypeState -> TypeState
enterBlock (varEnv, retType) =
  (Map.map (\(t, _, w) -> (t, HigherBlock, w)) varEnv, retType)

addVariable :: IsReadOnly -> Arg -> TypeMonad ()
addVariable isRO (Arg declType ident) = do
  (varEnv, retType) <- get
  put (Map.insert ident (declType, CurrentBlock, isRO) varEnv, retType)

checkDuplicateVariable :: Item -> TypeMonad ()
checkDuplicateVariable (Init ident@(Ident name) _) = do
  (varEnv, _) <- get
  case Map.lookup ident varEnv of
    Just (_, CurrentBlock, _) ->
      errorWithoutStackTrace $ "Redeclaration of variable `" ++ name ++ "`."
    _ -> return ()

makeState :: Type -> TypeState
makeState retType = (Map.empty, retType)

addFunToEnv :: FunEnv -> TopDef -> IO FunEnv
addFunToEnv env fun@(FnDef _ ident@(Ident name) _ _) = do
  when (Map.member ident env) $
    errorWithoutStackTrace $ "Redeclaration of function `" ++ name ++ "`."
  return $ Map.insert ident fun env

checkProg :: Program -> IO ()
checkProg (Program topDefs) = do
  env <- foldM addFunToEnv Interpreter.makeFunEnv topDefs
  checkMain env
  forM_ topDefs $ \(FnDef funType ident _ _) ->
    runRWST (checkFun (env ! ident)) env $ makeState funType

checkMain :: FunEnv -> IO ()
checkMain env = case Map.lookup (Ident "main") env of
  Nothing -> errorWithoutStackTrace "Function `main` not defined."
  Just fnDef -> case fnDef of
    FnDef Int _ [] _ -> return ()
    FnDef Int _ [Arg Str _] _ -> return ()
    FnDef Int _ _ _ ->
      errorWithoutStackTrace "Function `main` takes illegal arguments."
    FnDef{} -> errorWithoutStackTrace "Function `main` is not of type `int`."

checkFun :: TopDef -> TypeMonad ()
checkFun (FnDef funType _ args (Block stmts)) = do
  forM_ args $ addVariable Writable
  forM_ stmts checkStmt

checkStmt :: Stmt -> TypeMonad ()
checkStmt stmt = case stmt of
  Empty -> return ()
  BStmt (Block stmts) -> do
    oldState <- get
    put $ enterBlock oldState
    forM_ stmts checkStmt
    put oldState
  Decl t items -> do
    types <- forM [e | (Init _ e) <- items] checkExpr
    forM_ types $ assert stmt t
    forM_ items checkDuplicateVariable
    forM_ [Arg t ident | (Init ident _) <- items] $ addVariable Writable
  Ass ident expr -> do
    varType <- getVarType ident
    exprType <- checkExpr expr
    assertWritable ident
    void $ assert stmt varType exprType
  Incr ident -> do
    t <- getVarType ident
    assertWritable ident
    void $ assert stmt t Int
  Decr ident -> do
    t <- getVarType ident
    assertWritable ident
    void $ assert stmt t Int
  Ret expr -> do
    (_, retType) <- get
    t <- checkExpr expr
    void $ assert stmt retType t
  VRet -> do
    (_, retType) <- get
    void $ assert stmt retType Void
  Cond expr body -> checkCondWhile expr body
  CondElse expr body1 body2 -> do
    t <- checkExpr expr
    assert stmt t Bool
    checkStmt $ Interpreter.wrapInBlock body1
    checkStmt $ Interpreter.wrapInBlock body2
  While expr body -> checkCondWhile expr body
  SExp expr -> void $ checkExpr expr
  Print exprs -> do
    types <- forM exprs checkExpr
    forM_ types (assertMany stmt [Int, Bool, Str])
  For t ident expr1 _ expr2 body -> do
    assert stmt t Int
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    assert stmt t1 Int
    assert stmt t2 Int
    oldState <- get
    addVariable ReadOnly $ Arg t ident
    checkStmt $ Interpreter.wrapInBlock body
    put oldState
  where
    checkCondWhile expr body = do
      t <- checkExpr expr
      assert stmt t Bool
      checkStmt $ Interpreter.wrapInBlock body

-- Recursively check subexpressions of this expression, and if they are fine,
-- return its type. Throw an error otherwise.
checkExpr :: Expr -> TypeMonad Type
checkExpr expr = case expr of
  EVar ident -> getVarType ident
  ELitInt _ -> return Int
  ELitTrue -> return Bool
  ELitFalse -> return Bool
  EApp ident exprArgs -> do
    funEnv <- ask
    evalArgs <- forM exprArgs checkExpr
    let (Ident name) = ident
    case Map.lookup ident funEnv of
      Nothing ->
        errorWithoutStackTrace $ "Function `" ++ name ++ "` was not declared."
      Just (FnDef t _ funArgs _) ->
        if length evalArgs == length funArgs
          then do
            forM_ [(t1, t2) | t1 <- evalArgs | (Arg t2 _) <- funArgs]
              $ uncurry $ assert expr
            return t
          else errorWithoutStackTrace $
            "Invalid number of arguments passed to `" ++ name ++ "` in:\n"
            ++ printTree expr
  EString _ -> return Str
  Neg e -> do
    t <- checkExpr e
    assert expr t Int
  Not e -> do
    t <- checkExpr e
    assert expr t Bool
  EMul e1 _ e2 -> do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    assert expr t1 Int
    assert expr t2 Int
  EAdd e1 op e2 -> do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    assert expr t1 t2
    case op of
      Plus -> assertMany expr [Int, Str] t1
      Minus -> assert expr t1 Int
  ERel e1 op e2 -> do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    if op == EQU || op == NE
      then assertMany expr [Int, Str, Bool] t1
      else assertMany expr [Int, Str] t1
    assert expr t1 t2
    return Bool
  EAnd e1 e2 -> checkBoolOp expr e1 e2
  EOr e1 e2 -> checkBoolOp expr e1 e2
  where
    checkBoolOp expr e1 e2 = do
      t1 <- checkExpr e1
      t2 <- checkExpr e2
      assert expr t1 Bool
      assert expr t2 Bool

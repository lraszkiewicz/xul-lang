module TypeChecker where

import Control.Monad.RWS
import Data.Map (Map, (!))
import qualified Interpreter

import AbsXul
import ParXul

type FunEnv = Interpreter.FunEnv

type TypeVarEnv = Map Ident (Type, Int)
type TypeState = (TypeVarEnv, Int)

type TypeMonad = RWST FunEnv () TypeState IO

checkProg :: Program -> IO ()
checkProg program = return ()

assert :: Type -> Type -> Expr -> TypeMonad Type
assert t1 t2 e = if t1 == t2 then return t1
  else error $ "Type " ++ show t1 ++ " does not match type " ++ show t2
               ++ " in expression " ++ show e

assertMany :: Type -> [Type] -> Expr -> TypeMonad Type
assertMany t ts e = if t `elem` ts then return t
  else error $ "Type " ++ show t ++ " is not one of " ++ show ts
               ++ " in expression " ++ show e

checkStmt :: Stmt -> TypeMonad ()
checkStmt stmt = case stmt of
  Empty -> return ()
  BStmt block -> do
    -- TODO
    return ()
  -- TODO

-- Recursively check subexpressions of this expression, and if they are fine,
-- return its type. Throw an error otherwise.
checkExpr :: Expr -> TypeMonad Type
checkExpr expr = case expr of
  EVar ident -> do
    (varEnv, _) <- get
    let (t, _) = varEnv ! ident
    return t
  ELitInt _ -> return Int
  ELitTrue -> return Bool
  ELitFalse -> return Bool
  EApp ident _ -> do
    funEnv <- ask
    let (FnDef t _ _ _) = funEnv ! ident
    return t
  EString _ -> return Str
  Neg e -> do
    t <- checkExpr e
    assert t Bool expr
  Not e -> do
    t <- checkExpr e
    assert t Bool expr
  EMul e1 _ e2 -> do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    assert t1 Int expr
    assert t2 Int expr
  EAdd e1 op e2 -> do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    assert t1 t2 expr
    case op of
      Plus -> assertMany t1 [Int, Str] expr
      Minus -> assert t1 Int expr
  ERel e1 _ e2 -> do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    assertMany t1 [Int, Str] expr
    assert t1 t2 expr
  EAnd e1 e2 -> checkBoolOp e1 e2 expr
  EOr e1 e2 -> checkBoolOp e1 e2 expr

checkBoolOp :: Expr -> Expr -> Expr -> TypeMonad Type
checkBoolOp e1 e2 expr = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  assert t1 Bool expr
  assert t2 Bool expr

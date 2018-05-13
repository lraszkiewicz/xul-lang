module Interpreter where

import Control.Monad.RWS
import Data.Bool
import Data.Map (Map, (!))
import qualified Data.Map as Map

import AbsXul
import ParXul

type FunEnv = Map Ident TopDef
type VarEnv = Map Ident Int
type Env = (FunEnv, VarEnv)

type Store = Map Int Expr
type State = (Int, Store)

type ProgMonad = RWST Env () State IO

interpret :: Program -> IO ()
interpret (Program topDefs) = print "Hello"
  -- void $ runRWST
  --     ()
  --     (foldr (\def@(FnDef _ ident _ _) -> Map.insert ident def) Map.empty)
  --     ())

emptyEnv :: Env
emptyEnv = (Map.empty, Map.empty)

emptyState :: State
emptyState = (0, Map.empty)

initVariable :: (Arg, Expr) -> (Env, State) -> (Env, State)
initVariable (Arg _ ident, expr) ((funEnv, varEnv), (newloc, store)) =
  ((funEnv, Map.insert ident newloc varEnv),
  (newloc + 1, Map.insert newloc expr store))

execFun :: TopDef -> [Expr] -> ProgMonad Expr
execFun (FnDef _ _ args block) exprArgs = do
  evalArgs <- forM exprArgs eval
  oldState <- get
  let (funEnv, funState) = foldr initVariable (emptyEnv, emptyState) $ zip args evalArgs
  put funState
  return ELitTrue

eval :: Expr -> ProgMonad Expr
eval e = case e of
  EVar ident -> do
    (_, varEnv) <- ask
    (_, store) <- get
    return $ store ! (varEnv ! ident)
  EApp ident args -> do
    (funEnv, _) <- ask
    execFun (funEnv ! ident) args
  ELitInt _ -> return e
  ELitTrue -> return e
  ELitFalse -> return e
  EString _ -> return e
  Neg e1 -> do
    ELitInt n <- eval e1
    return $ ELitInt $ -n
  Not e1 -> do
    e2 <- eval e1
    case e2 of
      ELitTrue -> return ELitFalse
      ELitFalse -> return ELitTrue
  EMul e1 op e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    case (e3, op, e4) of
      (ELitInt n1, Times, ELitInt n2) -> return $ ELitInt $ n1 * n2
      (ELitInt n1, Div, ELitInt n2) -> return $ ELitInt $ div n1 n2
      (ELitInt n1, Mod, ELitInt n2) -> return $ ELitInt $ mod n1 n2
  EAdd e1 op e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    case (e3, op, e4) of
      (ELitInt n1, Plus, ELitInt n2) -> return $ ELitInt $ n1 + n2
      (EString s1, Plus, EString s2) -> return $ EString $ s1 ++ s2
      (ELitInt n1, Minus, ELitInt n2) -> return $ ELitInt $ n1 - n2
  ERel e1 op e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    op2 <- case op of
      LTH -> return (<)
      LE -> return (<=)
      GTH -> return (>)
      GE -> return (>=)
      EQU -> return (==)
      NE -> return (/=)
    let res = op2 e3 e4
    return $ if res then ELitTrue else ELitFalse
  EAnd e1 e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    return $ if e3 == ELitTrue && e4 == ELitTrue then ELitTrue else ELitFalse
  EOr e1 e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    return $ if e3 == ELitTrue || e4 == ELitTrue then ELitTrue else ELitFalse

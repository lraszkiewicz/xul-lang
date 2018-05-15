{-# LANGUAGE ParallelListComp #-}

module Interpreter where

import Control.Monad.RWS
import Data.Maybe
import Data.Bool
import Data.Map (Map, (!))
import qualified Data.Map as Map

import AbsXul
import ParXul

type FunEnv = Map Ident TopDef

type VarEnv = Map Ident Int
type Store = Map Int Expr
-- State: (varEnv, newloc, store, ret)
--- varEnv - mapping of variables to a place in Store
--- newloc - next available memory location
--- store - mapping of memory locations to values
--- ret - Just $ return value of the currently executed function
---       or Nothing if not yet returned
type State = (VarEnv, Int, Store, Maybe Expr)

type ProgMonad = RWST FunEnv () State IO

interpret :: Program -> String -> IO ()
interpret (Program topDefs) arg = do
  let funEnv = foldr
        (\def@(FnDef _ ident _ _) -> Map.insert ident def) makeFunEnv topDefs
  void $ runRWST (execFun (funEnv ! Ident "main") [EString arg]) funEnv makeState

makeFunEnv :: FunEnv
makeFunEnv = Map.fromList [
    (Ident "intToString",
     FnDef Str (Ident "intToString") [Arg Int (Ident "n")] (Block [])),
    (Ident "stringToInt",
     FnDef Int (Ident "stringToInt") [Arg Str (Ident "s")] (Block []))
  ]

makeState :: State
makeState = (Map.empty, 0, Map.empty, Nothing)

initVariable :: Item -> State -> State
initVariable (Init ident expr) (varEnv, newloc, store, ret) =
  (Map.insert ident newloc varEnv,
   newloc + 1,
   Map.insert newloc expr store,
   ret)

execFun :: TopDef -> [Expr] -> ProgMonad Expr
execFun (FnDef _ (Ident "intToString") _ _) [exprArg] = do
  evalArg <- eval exprArg
  case evalArg of
    ELitInt n -> return $ EString $ show n
execFun (FnDef _ (Ident "stringToInt") _ _) [exprArg] = do
  evalArg <- eval exprArg
  case evalArg of
    EString s -> return $ ELitInt $ read s
execFun (FnDef funType (Ident funName) args block) exprArgs = do
  evalArgs <- forM exprArgs eval
  oldState <- get
  let inits = [Init ident expr | (Arg _ ident) <- args | expr <- evalArgs]
  put $ foldr initVariable makeState inits
  execStmt $ BStmt block
  (_, _, _, funRet) <- get
  put oldState
  case funRet of
    Just retVal -> return retVal
    Nothing -> if funType == Void
               then return ELitTrue
               else error $ "Non-void function `" ++ funName ++
                            "` did not return a value."

valToString :: Expr -> String
valToString val = case val of
  ELitInt n -> show n
  ELitTrue -> "true"
  ELitFalse -> "false"
  EString s -> s

-- Assumes that the loop counter was added to varEnv earlier.
execForLoop :: Stmt -> ProgMonad ()
execForLoop (For t ident valStart ord exprEnd body) = do
  ELitInt valCounter <- eval (EVar ident)
  ELitInt valEnd <- eval exprEnd
  let compOp = if ord == OrdUp then (<=) else (>=) :: Integer -> Integer -> Bool
  when (compOp valCounter valEnd) $ do
    execStmt body
    execStmt $ (if ord == OrdUp then Incr else Decr) ident
    execForLoop (For t ident valStart ord exprEnd body)

-- In conditional statements and loops, the body will be wrapped in a block
-- statement if it already wasn't. This is to ensure that `if (...) int a = 1;`
-- will have its own environment, like `if (...) {int a = 1;}` would.
wrapInBlock :: Stmt -> Stmt
wrapInBlock stmt = case stmt of
  BStmt _ -> stmt
  _ -> BStmt $ Block [stmt]

execStmt :: Stmt -> ProgMonad ()
execStmt stmt = do
  oldState@(varEnv, newloc, store, ret) <- get
  when (isNothing ret) $ case stmt of
    Empty -> return ()
    BStmt (Block stmts) -> do
      forM_ stmts execStmt
      (_, newNewloc, newStore, newRet) <- get
      put (varEnv, newNewloc, newStore, newRet)
    Decl _ vars -> do
      vals <- forM [expr | (Init _ expr) <- vars] eval
      let inits = [Init ident val | (Init ident _) <- vars | val <- vals]
      put $ foldr initVariable oldState inits
    Ass ident expr -> do
      val <- eval expr
      let newStore = Map.insert (varEnv ! ident) val store
      put (varEnv, newloc, newStore, ret)
    Incr ident -> do
      let (ELitInt val) = store ! (varEnv ! ident)
      let newStore = Map.insert (varEnv ! ident) (ELitInt $ val + 1) store
      put (varEnv, newloc, newStore, ret)
    Decr ident -> do
      let (ELitInt val) = store ! (varEnv ! ident)
      let newStore = Map.insert (varEnv ! ident) (ELitInt $ val - 1) store
      put (varEnv, newloc, newStore, ret)
    Ret expr -> do
      val <- eval expr
      put (varEnv, newloc, store, Just val)
    -- The return value from VRet will be ignored, it just can't be Nothing.
    VRet -> put (varEnv, newloc, store, Just ELitTrue)
    Cond expr body -> do
      val <- eval expr
      when (val == ELitTrue) $ execStmt $ wrapInBlock body
    CondElse expr bodyIf bodyElse -> do
      val <- eval expr
      execStmt $ if val == ELitTrue
                 then wrapInBlock bodyIf
                 else wrapInBlock bodyElse
    loop@(While expr body) -> do
      val <- eval expr
      when (val == ELitTrue) $ do
        execStmt $ wrapInBlock body
        execStmt loop
    SExp expr -> void $ eval expr
    Print exprs -> do
      vals <- forM exprs eval
      lift $ putStr $ unwords $ map valToString vals
      lift $ putStr "\n"
    For t ident exprStart ord exprEnd body -> do
      ELitInt valStart <- eval exprStart
      let loopEnv = initVariable (Init ident (ELitInt valStart)) oldState
      put loopEnv
      execForLoop $
        For t ident (ELitInt valStart) ord exprEnd $ wrapInBlock body
      (_, newNewloc, newStore, newRet) <- get
      put (varEnv, newNewloc, newStore, newRet)

eval :: Expr -> ProgMonad Expr
eval e = case e of
  EVar ident -> do
    (varEnv, _, store, _) <- get
    return $ store ! (varEnv ! ident)
  ELitInt _ -> return e
  ELitTrue -> return e
  ELitFalse -> return e
  EApp ident args -> do
    funEnv <- ask
    execFun (funEnv ! ident) args
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
    let op2 = case op of
          LTH -> (<)
          LE -> (<=)
          GTH -> (>)
          GE -> (>=)
          EQU -> (==)
          NE -> (/=)
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

{-# LANGUAGE ParallelListComp #-}

module Interpreter where

import Control.Monad.RWS
import Data.Maybe
import Data.Bool
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Debug.Trace

import AbsXul
import ParXul

-- TODO:
-- * check if point 8 from scoring is done correctly
-- * maybe change the RWST monad to something else
-- * change return type of eval

data Value
  = ValInt Integer
  | ValFalse
  | ValTrue
  | ValString String
  deriving (Eq, Ord)

data InitValue = InitValue Ident Value

type FunEnv = Map Ident TopDef

type VarEnv = Map Ident Int
type Store = Map Int Value
data LoopState
  = LoopNone
  | LoopBreak
  | LoopContinue
-- State: (varEnv, newloc, store, ret, loopState)
--- varEnv - mapping of variables to a place in Store
--- newloc - next available memory location
--- store - mapping of memory locations to values
--- ret - Just $ return value of the currently executed function
---       or Nothing if not yet returned
--- loopState - LoopBreak or LoopContinue when break or continue have been used
---             in the loop's current iteration, LoopNone when they haven't or
---             when a loop is not being currently executed
type State = (VarEnv, Int, Store, Maybe Value, LoopState)

type ProgMonad = RWST FunEnv () State IO

interpret :: Program -> String -> IO Integer
interpret (Program topDefs) arg = do
  let funEnv = foldr
        (\def@(FnDef _ ident _ _) -> Map.insert ident def) makeFunEnv topDefs
  (ValInt retVal, _, _) <-
    runRWST (execFun (funEnv ! Ident "main") [EString arg]) funEnv makeState
  return retVal

makeFunEnv :: FunEnv
makeFunEnv = Map.fromList [
    (Ident "intToString",
     FnDef Str (Ident "intToString") [Arg Int (Ident "n")] (Block [])),
    (Ident "stringToInt",
     FnDef Int (Ident "stringToInt") [Arg Str (Ident "s")] (Block []))
  ]

makeState :: State
makeState = (Map.empty, 0, Map.empty, Nothing, LoopNone)

initVariable :: InitValue -> State -> State
initVariable (InitValue ident value) (varEnv, newloc, store, ret, loopState) =
  (Map.insert ident newloc varEnv,
   newloc + 1,
   Map.insert newloc value store,
   ret,
   loopState)

execFun :: TopDef -> [Expr] -> ProgMonad Value
execFun (FnDef _ (Ident "intToString") _ _) [exprArg] = do
  evalArg <- eval exprArg
  case evalArg of
    ValInt n -> return $ ValString $ show n
execFun (FnDef _ (Ident "stringToInt") _ _) [exprArg] = do
  evalArg <- eval exprArg
  case evalArg of
    ValString s -> case readMaybe s of
      Just x -> return $ ValInt x
      Nothing -> errorWithoutStackTrace $
        show s ++ " cannot be parsed as an integer."
execFun (FnDef funType (Ident funName) args block) exprArgs = do
  evalArgs <- forM exprArgs eval
  oldState <- get
  let inits = [InitValue ident val | (Arg _ ident) <- args | val <- evalArgs]
  put $ foldr initVariable makeState inits
  execStmt $ BStmt block
  (_, _, _, funRet, _) <- get
  put oldState
  case funRet of
    Just retVal -> return retVal
    Nothing -> if funType == Void
      then return ValTrue
      else errorWithoutStackTrace $
        "Non-void function `" ++ funName ++ "` did not return a value."

valToString :: Value -> String
valToString val = case val of
  ValInt n -> show n
  ValTrue -> "true"
  ValFalse -> "false"
  ValString s -> s

-- In conditional statements and loops, the body will be wrapped in a block
-- statement if it already wasn't. This is to ensure that `if (...) int a = 1;`
-- will have its own environment, like `if (...) {int a = 1;}` would.
wrapInBlock :: Stmt -> Stmt
wrapInBlock stmt = case stmt of
  BStmt _ -> stmt
  _ -> BStmt $ Block [stmt]

isBreak :: LoopState -> Bool
isBreak loopState = case loopState of
  LoopBreak -> True
  _ -> False

isLoopNone :: LoopState -> Bool
isLoopNone loopState = case loopState of
  LoopNone -> True
  _ -> False

setLoopNone :: ProgMonad ()
setLoopNone = do
  (varEnv, newloc, store, ret, _) <- get
  put (varEnv, newloc, store, ret, LoopNone)

-- Assumes that the loop counter was added to varEnv earlier.
execForLoop :: Stmt -> ProgMonad ()
execForLoop (For t ident valStart ord exprEnd body) = do
  ValInt valCounter <- eval (EVar ident)
  ValInt valEnd <- eval exprEnd
  let compOp = if ord == OrdUp then (<=) else (>=) :: Integer -> Integer -> Bool
  when (compOp valCounter valEnd) $ do
    setLoopNone
    execStmt body
    (_, _, _, _, loopState) <- get
    when (not $ isBreak loopState) $ do
      setLoopNone
      execStmt $ (if ord == OrdUp then Incr else Decr) ident
      execForLoop (For t ident valStart ord exprEnd body)

execStmt :: Stmt -> ProgMonad ()
execStmt stmt = do
  oldState@(varEnv, newloc, store, ret, loopState) <- get
  when (isNothing ret && isLoopNone loopState) $ case stmt of
    Empty -> return ()
    BStmt (Block stmts) -> do
      forM_ stmts execStmt
      (_, newNewloc, newStore, newRet, newLoopState) <- get
      put (varEnv, newNewloc, newStore, newRet, newLoopState)
    Decl _ vars -> do
      vals <- forM [expr | (Init _ expr) <- vars] eval
      let inits = [InitValue ident val | (Init ident _) <- vars | val <- vals]
      put $ foldr initVariable oldState inits
    Ass ident expr -> do
      val <- eval expr
      let newStore = Map.insert (varEnv ! ident) val store
      put (varEnv, newloc, newStore, ret, loopState)
    Incr ident -> do
      let (ValInt val) = store ! (varEnv ! ident)
      let newStore = Map.insert (varEnv ! ident) (ValInt $ val + 1) store
      put (varEnv, newloc, newStore, ret, loopState)
    Decr ident -> do
      let (ValInt val) = store ! (varEnv ! ident)
      let newStore = Map.insert (varEnv ! ident) (ValInt $ val - 1) store
      put (varEnv, newloc, newStore, ret, loopState)
    Ret expr -> do
      val <- eval expr
      put (varEnv, newloc, store, Just val, loopState)
    -- The return value from VRet will be ignored, it just can't be Nothing.
    VRet -> put (varEnv, newloc, store, Just ValTrue, loopState)
    Cond expr body -> do
      val <- eval expr
      when (val == ValTrue) $ execStmt $ wrapInBlock body
    CondElse expr bodyIf bodyElse -> do
      val <- eval expr
      execStmt $
        if val == ValTrue then wrapInBlock bodyIf else wrapInBlock bodyElse
    loop@(While expr body) -> do
      val <- eval expr
      when (val == ValTrue) $ do
        setLoopNone
        execStmt $ wrapInBlock body
        (_, _, _, _, newLoopState) <- get
        setLoopNone
        when (not $ isBreak newLoopState) $ execStmt loop
    SExp expr -> void $ eval expr
    Print exprs -> do
      vals <- forM exprs eval
      lift $ putStr $ unwords $ map valToString vals
      lift $ putStr "\n"
    For t ident exprStart ord exprEnd body -> do
      ValInt valStart <- eval exprStart
      let loopEnv = initVariable (InitValue ident (ValInt valStart)) oldState
      put loopEnv
      execForLoop $
        For t ident (ELitInt valStart) ord exprEnd $ wrapInBlock body
      (_, newNewloc, newStore, newRet, _) <- get
      put (varEnv, newNewloc, newStore, newRet, LoopNone)
    Break -> put (varEnv, newloc, store, ret, LoopBreak)
    Continue -> put (varEnv, newloc, store, ret, LoopContinue)

eval :: Expr -> ProgMonad Value
eval e = case e of
  EVar ident -> do
    (varEnv, _, store, _, _) <- get
    return $ store ! (varEnv ! ident)
  ELitInt n -> return $ ValInt n
  ELitTrue -> return ValTrue
  ELitFalse -> return ValFalse
  EApp ident args -> do
    funEnv <- ask
    execFun (funEnv ! ident) args
  EString s -> return $ ValString s
  Neg e1 -> do
    ValInt n <- eval e1
    return $ ValInt $ -n
  Not e1 -> do
    e2 <- eval e1
    case e2 of
      ValTrue -> return ValFalse
      ValFalse -> return ValTrue
  EMul e1 op e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    case (e3, op, e4) of
      (ValInt n1, Times, ValInt n2) -> return $ ValInt $ n1 * n2
      (_, _, ValInt 0) -> errorWithoutStackTrace "Division by 0."
      (ValInt n1, Div, ValInt n2) -> return $ ValInt $ div n1 n2
      (ValInt n1, Mod, ValInt n2) -> return $ ValInt $ mod n1 n2
  EAdd e1 op e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    case (e3, op, e4) of
      (ValInt n1, Plus, ValInt n2) -> return $ ValInt $ n1 + n2
      (ValString s1, Plus, ValString s2) -> return $ ValString $ s1 ++ s2
      (ValInt n1, Minus, ValInt n2) -> return $ ValInt $ n1 - n2
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
    return $ if res then ValTrue else ValFalse
  EAnd e1 e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    return $ if e3 == ValTrue && e4 == ValTrue then ValTrue else ValFalse
  EOr e1 e2 -> do
    e3 <- eval e1
    e4 <- eval e2
    return $ if e3 == ValTrue || e4 == ValTrue then ValTrue else ValFalse

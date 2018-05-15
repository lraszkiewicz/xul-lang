import Data.List
import System.Environment
import System.IO
import System.Exit

import AbsXul
import ErrM
import Interpreter
import TypeChecker
import ParXul

main :: IO ()
main = do
  args <- getArgs
  let progArg = case filter (isPrefixOf "--arg=") args of
        x:_ -> drop 6 x
        [] -> ""
  progFile <- if null args then return stdin else openFile (head args) ReadMode
  progCode <- hGetContents progFile
  prog <- case pProgram (myLexer progCode) of
    Ok program -> return program
    Bad err -> error $ "Parsing error: " ++ err
  checkProg prog
  exitCode <- interpret prog progArg
  if exitCode == 0
    then exitSuccess
    else exitWith $ ExitFailure $ fromInteger exitCode

import Data.List
import System.Environment
import System.IO

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
    Bad err -> do
      putStr $ "Parsing error: " ++ err ++ "\n"
      return $ Program []
  checkProg prog
  interpret prog progArg

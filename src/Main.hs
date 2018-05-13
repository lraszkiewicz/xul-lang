import System.Environment
import System.IO

import AbsXul
import ErrM
import Interpreter
import ParXul

main :: IO ()
main = do
  args <- getArgs
  progFile <- if null args then return stdin else openFile (head args) ReadMode
  progCode <- hGetContents progFile
  prog <- case pProgram (myLexer progCode) of
    Ok program -> return program
    Bad err -> do
      print $ "Parsing error: " ++ err
      return $ Program []
  interpret prog

module Main where

import System.Environment
import System.Console.Haskeline

import Ast
import Parser
import Eval
import Errors

import Control.Monad
import Control.Monad.Except

interpret :: String -> IO String
interpret input = return $ either show show $ runExcept $ readExpr input >>= eval

evalFile :: String -> IO ()
evalFile s = readFile s >>= interpret >>= putStrLn

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      result <- getInputLine "scheme> "
      case result of
        Nothing -> return ()
        Just "quit" -> return ()
        Just s -> liftIO (interpret s) >>= outputStrLn >> loop

main :: IO ()
main = do
  args <- getArgs
  case (length args) of
    0 -> repl
    1 -> evalFile $ args !! 0
    otherwise -> putStrLn "Error! Repl or file mode not selected"

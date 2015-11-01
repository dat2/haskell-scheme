module Main where

import System.Environment
import System.Console.Haskeline

import Ast
import Parser
import Env
import Eval
import Errors

import Control.Monad
import Control.Monad.Except

interpret :: Env -> String -> IO String
interpret env input = do
  e <- runExceptT $ liftThrows (readExpr input) >>= eval env
  return $ either show show e

evalFile :: String -> IO ()
evalFile s = do
  env <- newEnv
  readFile s >>= interpret env >>= putStrLn

repl :: IO ()
repl = runInputT defaultSettings $ do { env <- liftIO newEnv; loop env }
  where
    loop env = do
      result <- getInputLine "scheme> "
      case result of
        Nothing -> return ()
        Just "quit" -> return ()
        Just s -> liftIO (interpret env s) >>= outputStrLn >> loop env

main :: IO ()
main = do
  args <- getArgs
  case (length args) of
    0 -> repl
    1 -> evalFile $ args !! 0
    otherwise -> putStrLn "Error! Repl or file mode not selected"

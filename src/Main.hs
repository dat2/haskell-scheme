module Main where

import Eval
import Parser
import Types

import Control.Monad
import Control.Monad.Except

import System.Environment
import System.Console.Haskeline

interpret :: Env -> String -> IO String
interpret env input = do
  e <- runExceptT $ liftThrows (readProgram "lisp" input) >>= liftM last . mapM (eval env)
  return $ either show show e

evalFile :: [String] -> IO ()
evalFile (filename:args) = do
  env <- nativeBindings >>= flip bindVars [("args", List $ map String args)]
  readFile filename >>= interpret env >>= putStrLn

repl :: IO ()
repl = runInputT defaultSettings $ liftIO nativeBindings >>= loop
  where
    loop env = do
      result <- getInputLine "lisp> "
      case result of
        Nothing -> return ()
        Just "quit" -> return ()
        Just s -> liftIO (interpret env s) >>= outputStrLn >> loop env

main :: IO ()
main = do
  args <- getArgs
  if null args
     then repl
     else evalFile args

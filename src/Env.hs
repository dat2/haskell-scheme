module Env where

import Ast
import Errors

import Control.Monad.Except

import Data.IORef

type Env = IORef [(String, IORef LispVal)]

newEnv :: IO Env
newEnv = newIORef []

type IOThrows = ExceptT LispError IO

liftThrows :: Throws a -> IOThrows a
liftThrows t =
  case runExcept t of
    Left err -> throwError err
    Right val -> return val

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrows LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrows LispVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef val))
        (lookup var env)
  return val

defineVar :: Env -> String -> LispVal -> IOThrows LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value
     else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value

-- bindVars :: Env -> [(String, LispVal)] -> IO Env


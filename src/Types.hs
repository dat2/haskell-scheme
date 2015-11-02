{-# LANGUAGE NamedFieldPuns #-}

module Types where

import Control.Monad.Except
import Data.IORef
import Text.ParserCombinators.Parsec.Error

{- Language primitives section -}
data LispVal =
    Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Character Char
  | Bool Bool
  | NativeFunc (Env -> [LispVal] -> IOThrows LispVal)
  | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }

instance Show LispVal where
  show (Atom n) = n
  show (List ls) = "(" ++ unwordsList ls ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Number i) = show i
  show (String s) = "\"" ++ s ++ "\""
  show (Character c) = "#\\" ++ [c]
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (NativeFunc _) = "<native>"
  show (Func { params, vararg, body, closure }) =
    "(lambda (" ++ unwordsList params ++
      case vararg of
        Nothing -> ") ...)"
        Just arg -> " . " ++ arg ++ ") ...)"

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

{- Error section -}
data LispError =
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  | NonExhaustive
  | IndexError Int Int
  | ErrorList [LispError]

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values (" ++ unwordsList found ++ ")"
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (Default s) = s
  show (NonExhaustive) = "A case or cond condition is non exhaustive"
  show (IndexError n len) = show n ++ " is out of bounds of [0," ++ show len ++ ")"
  show (ErrorList es) = unlines $ map show es

type Throws = Except LispError

trapError action = catchError action (return . show)

{- Environment section -}
type Env = IORef [(String, IORef LispVal)]

newEnv :: IO Env
newEnv = newIORef []

type IOThrows = ExceptT LispError IO

-- lift a regular Throws to an IOThrows
liftThrows :: Throws a -> IOThrows a
liftThrows t =
  case runExcept t of
    Left err -> throwError err
    Right val -> return val

-- checks if the value is in the environment
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- gets the named ioref's value
getVar :: Env -> String -> IOThrows LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

-- sets a named ioref's value
setVar :: Env -> String -> LispVal -> IOThrows LispVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef val))
        (lookup var env)
  return val

-- adds a named ioref to the env ioref
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

-- reads the env ref, adds all the bindings to it, then makes a new ref for this
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    -- extendEnv ([(string,lispVal)] -> [(string, ioref)]) ++ env
    extendEnv bindings env = liftM (++ env) $ mapM addBinding bindings
    -- add binding, takes (string, lispval) -> (string, ioref)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

module Eval where

import Ast
import Errors

import Control.Monad.Except

eval :: LispVal -> Throws LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom f : args)) = mapM eval args >>= apply f
eval val = return val

-- apply the function to its parameters
apply :: String -> [LispVal] -> Throws LispVal
apply f args = maybe  (throwError $ NotFunction "Unrecognized primitive function args" f)
                      ($ args)
                      (lookup f primitives)

-- a list of default functions
primitives :: [(String, [LispVal] -> Throws LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", string'),
              ("number?", number'),
              ("symbol?", symbol'),
              ("boolean?", boolean'),
              ("list?", list'),
              ("pair?", pair'),
              ("symbol->string", symbolToString),
              ("not", not')]

-- take a binary operator, apply it to a list
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Throws LispVal
numericBinop op []          = throwError $ NumArgs 2 []
numericBinop op single@[_]  = throwError $ NumArgs 2 single
numericBinop op params      = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> Throws Integer
unpackNum (Number n) = return n
unpackNum notNumber = throwError $ TypeMismatch "number" notNumber

string' :: [LispVal] -> Throws LispVal
string' [String _] = return $ Bool True
string' l@(_:_) = throwError $ NumArgs 1 l
string' _ = return $ Bool False

number' :: [LispVal] -> Throws LispVal
number' [Number _] = return $ Bool True
number' l@(_:_) = throwError $ NumArgs 1 l
number' _ = return $ Bool False

symbol' :: [LispVal] -> Throws LispVal
symbol' [Atom _] = return $ Bool True
symbol' l@(_:_) = throwError $ NumArgs 1 l
symbol' _ = return $ Bool False

boolean' :: [LispVal] -> Throws LispVal
boolean' [Bool _] = return $ Bool True
boolean' l@(_:_) = throwError $ NumArgs 1 l
boolean' _ = return $ Bool False

list' :: [LispVal] -> Throws LispVal
list' [List _] = return $ Bool True
list' l@(_:_) = throwError $ NumArgs 1 l
list' _ = return $ Bool False

pair' :: [LispVal] -> Throws LispVal
pair' (List (_:[]):_) = return $ Bool False
pair' (List (_:_):_) = return $ Bool True
pair' (DottedList [] _:_) = return $ Bool False
pair' (DottedList (_:_) _:_) = return $ Bool True
pair' _ = return $ Bool False

not' :: [LispVal] -> Throws LispVal
not' [Bool f] = return $ Bool $ not f
not' l@(_:_) = throwError $ NumArgs 1 l
not' _ = return $ Bool False

symbolToString :: [LispVal] -> Throws LispVal
symbolToString [Atom n] = return $ String n
symbolToString l@(_:_) = throwError $ NumArgs 1 l

--  Atom String
-- | List [LispVal]
-- | DottedList [LispVal] LispVal
-- | Character Char
-- | Bool Bool

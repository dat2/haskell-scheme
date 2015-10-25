module Errors where

import Ast

import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

data LispError =
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values (" ++ unwordsList found ++ ")"
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr

type Throws = Except LispError

trapError action = catchError action (return . show)

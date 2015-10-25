module Ast where

data LispVal =
    Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Character Char
  | Bool Bool

instance Show LispVal where
  show (Atom n) = n
  show (List ls) = "(" ++ unwordsList ls ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Number i) = show i
  show (String s) = "\"" ++ s ++ "\""
  show (Character c) = "#\\" ++ [c]
  show (Bool True) = "#t"
  show (Bool False) = "#f"

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

module Main where

import System.Environment

import Ast
import Parser
import Eval
import Errors

import Control.Monad
import Control.Monad.Except

interpret :: String -> Either LispError LispVal
interpret input = runExcept $ readExpr input >>= eval

main :: IO ()
main = getArgs >>= readFile . head >>= return . interpret >>= print

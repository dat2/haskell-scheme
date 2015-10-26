module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except

import Numeric
import Data.Char (digitToInt)

import Ast
import Errors

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- special characters
parseEscaped :: Parser Char
parseEscaped = do
  char '\\'
  x <- oneOf "\"ntr\\"
  return $ case x of
    '"' -> '\"'
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    '\\' -> '\\'

-- parse a string
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ( parseEscaped <|> noneOf "\"" )
  char '"'
  return $ String x

-- parse an atom
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    -- turn else into true to make life easy
    "else" -> Bool True
    _ -> Atom atom

-- taken from
-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: Num a => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

readNumber :: ReadS Integer -> String -> LispVal
readNumber r = Number . fst . head . r

-- parse a number
-- liftM :: (a -> r) -> m a -> m r
-- parsec is so annoying with back tracking
-- TODO hexadecimal [a-fA-F]
parseNumber :: Parser LispVal
parseNumber = do
  prefix <- option 'd' $ do { char '#'; oneOf "xobd" }

  ds <- many1 digit

  case prefix of
    -- decimal
    'd' -> return $ readNumber readDec ds
    -- octal
    'o' -> return $ readNumber readOct ds
    -- hex
    'x' -> return $ readNumber readHex ds
    -- binary
    'b' -> return $ readNumber readBin ds

-- parse a character literal
parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  liftM Character $ anyChar

-- lists :)
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- dotted lists
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- quoted
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- parse expression
parseExpr :: Parser LispVal
parseExpr =   try parseNumber
          <|> try parseChar
          <|> parseAtom
          <|> parseString
          <|> parseQuoted
          <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> Throws LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

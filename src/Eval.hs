{-# LANGUAGE ExistentialQuantification #-}
module Eval where

import Types
import Parser

import System.IO

import Control.Monad.Except

eval :: Env -> LispVal -> IOThrows LispVal
-- primitives
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
-- get the atom's value from the environment
eval env (Atom id) = getVar env id
-- quote
eval env (List [Atom "quote", val]) = return val
-- if
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    otherwise -> throwError $ TypeMismatch "boolean" pred
-- IOref
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var

-- function definition
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body

-- load returns the last one
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)

-- function application
eval env (List (f : args)) = do
  func <- eval env f
  argVals <- mapM (eval env) args
  apply func argVals

eval env val = return val

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

-- apply the function to its parameters
apply :: LispVal -> [LispVal] -> IOThrows LispVal
apply (NativeFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  -- if the params don't match the args
  if num params /= num args && varargs == Nothing
     -- throw an error saying what we are expecting
     then throwError $ NumArgs (num params) args
     -- else bind the vars in the closure
     else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    -- currying
    remainingArgs = drop (length params) args
    -- length -> toInteger
    num = toInteger . length
    -- take the last of eval'd body (monadically)
    evalBody env = liftM last $ mapM (eval env) body
    -- add var arg
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
      Nothing -> return env
apply (IOFunc func) args = func args
apply val args = throwError $ NotFunction "The value is not a function" $ show val

-- bound functions
nativeBindings :: IO Env
nativeBindings = newEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioNatives
                            ++ map (makeFunc NativeFunc) natives)
  where makeFunc constructor (name, func) = (name, constructor func)

-- a list of native functions
natives :: [(String, [LispVal] -> Throws LispVal)]
natives = [("+", numericBinop (+)),
          ("-", numericBinop (-)),
          ("*", numericBinop (*)),
          ("/", numericBinop div),
          ("mod", numericBinop mod),
          ("quotient", numericBinop quot),
          ("remainder", numericBinop rem),
          ("string?", isstring),
          ("number?", isnumber),
          ("symbol?", issymbol),
          ("boolean?", isboolean),
          ("list?", islist),
          ("pair?", ispair),
          ("symbol->string", symbolToString),
          ("not", not'),
          ("=", numBoolBinop (==)),
          ("<", numBoolBinop (<)),
          (">", numBoolBinop (>)),
          ("/=", numBoolBinop (/=)),
          (">=", numBoolBinop (>=)),
          ("<=", numBoolBinop (<=)),
          ("and", boolBoolBinop (&&)),
          ("or", boolBoolBinop (||)),
          ("string=?", stringBoolBinop (==)),
          ("string<?", stringBoolBinop (<)),
          ("string>?", stringBoolBinop (>)),
          ("string<=?", stringBoolBinop (<=)),
          ("string>=?", stringBoolBinop (>=)),
          ("car", car),
          ("cdr", cdr),
          ("cons", cons),
          ("eq?", iseqv),
          ("eqv?", iseqv),
          ("equal?", isequal),
          ("make-string", makestring),
          ("string", string),
          ("string-length", stringlength),
          ("string-ref", stringref),
          ("substring", undefined),
          ("string-length", undefined),
          ("string-append", undefined),
          ("string->list", undefined),
          ("list->string", undefined),
          ("string-copy", undefined)]

-- take a binary operator, apply it to a list
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Throws LispVal
numericBinop op []          = throwError $ NumArgs 2 []
numericBinop op single@[_]  = throwError $ NumArgs 2 single
numericBinop op params      = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> Throws a) -> (a -> a -> Bool) -> [LispVal] -> Throws LispVal
boolBinop unpacker op args =  if length args /= 2
                                    then throwError $ NumArgs 2 args
                                    else do
                                      [l,r] <- mapM unpacker args
                                      return $ Bool $ l `op` r

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
stringBoolBinop = boolBinop unpackString

unpackNum :: LispVal -> Throws Integer
unpackNum (Number n) = return n
unpackNum notNumber = throwError $ TypeMismatch "number" notNumber

unpackBool :: LispVal -> Throws Bool
unpackBool (Bool t) = return t
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackString :: LispVal -> Throws String
unpackString (String t) = return t
unpackString notString = throwError $ TypeMismatch "string" notString

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> Throws a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> Throws Bool
unpackEquals v1 v2 (AnyUnpacker unp) =
    do
      u1 <- unp v1
      u2 <- unp v2
      return $ u1 == u2
  `catchError` (const $ return False)

isstring :: [LispVal] -> Throws LispVal
isstring [String _] = return $ Bool True
isstring l@(_:_) = throwError $ NumArgs 1 l
isstring _ = return $ Bool False

isnumber :: [LispVal] -> Throws LispVal
isnumber [Number _] = return $ Bool True
isnumber l@(_:_) = throwError $ NumArgs 1 l
isnumber _ = return $ Bool False

issymbol :: [LispVal] -> Throws LispVal
issymbol [Atom _] = return $ Bool True
issymbol l@(_:_) = throwError $ NumArgs 1 l
issymbol _ = return $ Bool False

isboolean :: [LispVal] -> Throws LispVal
isboolean [Bool _] = return $ Bool True
isboolean l@(_:_) = throwError $ NumArgs 1 l
isboolean _ = return $ Bool False

islist :: [LispVal] -> Throws LispVal
islist [List _] = return $ Bool True
islist l@(_:_) = throwError $ NumArgs 1 l
islist _ = return $ Bool False

ispair :: [LispVal] -> Throws LispVal
ispair (List (_:[]):_) = return $ Bool False
ispair (List (_:_):_) = return $ Bool True
ispair (DottedList [] _:_) = return $ Bool False
ispair (DottedList (_:_) _:_) = return $ Bool True
ispair _ = return $ Bool False

not' :: [LispVal] -> Throws LispVal
not' [Bool f] = return $ Bool $ not f
not' l@(_:_) = throwError $ NumArgs 1 l
not' _ = return $ Bool False

symbolToString :: [LispVal] -> Throws LispVal
symbolToString [Atom n] = return $ String n
symbolToString l@(_:_) = throwError $ NumArgs 1 l

car :: [LispVal] -> Throws LispVal
car [(List (x:xs))] = return x
car [(DottedList (x:xs) _)] = return x
car [arg] = throwError $ TypeMismatch "pair" arg
car args = throwError $ NumArgs 1 args

cdr :: [LispVal] -> Throws LispVal
cdr [(List (x:xs))] = return $ List xs
cdr [(DottedList [_] r)] = return r
cdr [(DottedList (_:xs) x)] = return $ DottedList xs x
cdr [arg] = throwError $ TypeMismatch "pair" arg
cdr args = throwError $ NumArgs 1 args

cons :: [LispVal] -> Throws LispVal
cons [x, List []] = return $ List [x]
cons [x, List ls] = return $ List (x:ls)
cons [x, DottedList ls tail] = return $ DottedList (x:ls) tail
cons [x, y] = return $ DottedList [x] y
cons args = throwError $ NumArgs 2 args

iseqv :: [LispVal] -> Throws LispVal
iseqv [(Bool l), (Bool r)] = return $ Bool (l == r)
iseqv [(Number l), (Number r)] = return $ Bool (l == r)
iseqv [(String l), (String r)] = return $ Bool (l == r)
iseqv [(Character l), (Character r)] = return $ Bool (l == r)
iseqv [DottedList xs x, DottedList ys y] = iseqv [List (xs ++ [x]),List (ys ++ [y])]
iseqv [List xs, List ys] = do
  let zipped = zip xs ys
  eqvs <- mapM (\(x,y) -> iseqv [x,y]) zipped
  return $ Bool $ (length xs == length ys) && all isTrue eqvs
  where isTrue (Bool True) = True
        isTrue _ = False
iseqv [_,_] = return $ Bool False
iseqv args = throwError $ NumArgs 2 args

isequal :: [LispVal] -> Throws LispVal
isequal [v1, v2] = do
  primEquals <- liftM or $ mapM (unpackEquals v1 v2)
                [AnyUnpacker unpackNum, AnyUnpacker unpackString, AnyUnpacker unpackBool]
  (Bool x) <- iseqv [v1, v2]
  return $ Bool $ (x || primEquals)

isequal args = throwError $ NumArgs 2 args

makestring :: [LispVal] -> Throws LispVal
makestring [Number n, Character c] = return $ String $ replicate (fromIntegral n) c
makestring [n@(Number _)] =  makestring [n, Character ' ']
makestring args = throwError $ NumArgs 2 args

string :: [LispVal] -> Throws LispVal
string [Character c] = return $ String [c]
string ((Character c):cs) = do
  (String s) <- string cs
  return $ String $ c:s
string badArgs = throwError $ ErrorList $ map (TypeMismatch "character") badArgs

stringlength :: [LispVal] -> Throws LispVal
stringlength [String s] = return $ Number $ toInteger $ length s
stringlength [arg] = throwError $ TypeMismatch "string" arg
stringlength args = throwError $ NumArgs 1 args

-- checks if n e [min,max)
withinBounds :: Int -> Int -> Int -> Bool
withinBounds min max n = n >= min && n < max

sublist :: Int -> Int -> [a] -> [a]
sublist min max l = take (max - min) $ drop min l

-- TODO proper error
stringref :: [LispVal] -> Throws LispVal
stringref [String s, Number ninteger] =
  let
    n = fromIntegral ninteger
  in
    if withinBounds 0 (length s) n
      then return $ Character $ s !! n
      else throwError $ IndexError n (length s)
stringref [f,s] = throwError $ ErrorList [TypeMismatch "string" f, TypeMismatch "number" s]
stringref args = throwError $ NumArgs 2 args

-- TODO  proper error
-- TODO fix <<loop>>
substring :: [LispVal] -> Throws LispVal
substring [Number st, Number en, String s] =
  let
    start = fromIntegral st
    end = fromIntegral end
    len = length s
  in
    if withinBounds 0 len start && withinBounds 0 len end && start < end
      then return $ String $ sublist start end s
      else throwError $ ErrorList [IndexError start len, IndexError end len]
substring [f,s,t] = throwError $ ErrorList [TypeMismatch "number" f, TypeMismatch "number" s, TypeMismatch "string" t]
substring args = throwError $ NumArgs 3 args

ioNatives :: [(String, [LispVal] -> IOThrows LispVal)]
ioNatives = [ ("apply", applyProc),
              ("open-input-file", makePort ReadMode),
              ("open-output-file", makePort WriteMode),
              ("close-input-port", closePort),
              ("close-output-port", closePort),
              ("read", readProc),
              ("write", writeProc),
              ("read-contents", readContents),
              ("read-all", readAll) ]

-- apply f to list
applyProc :: [LispVal] -> IOThrows LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc args = throwError $ NotFunction "The value is not a function" (show args)

-- make a new IO port and return it
makePort :: IOMode -> [LispVal] -> IOThrows LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- close IO port
closePort :: [LispVal] -> IOThrows LispVal
closePort [Port port] = (liftIO $ hClose port) >> (return $ Bool True)
closePort _ = return $ Bool False

-- "eval"
readProc :: [LispVal] -> IOThrows LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . (readExpr)

-- write a value to stdout, or to the port
writeProc :: [LispVal] -> IOThrows LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

-- read the contents of the file
readContents :: [LispVal] -> IOThrows LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents badArg = throwError $ Default $ "An invalid value was given to read-contents: " ++ show badArg

-- readAllLines in file
load :: String -> IOThrows [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . (readProgram filename)

readAll :: [LispVal] -> IOThrows LispVal
readAll [String filename] = liftM List $ load filename
readAll badArg = throwError $ Default $ "An invalid value was given to read-all: " ++ show badArg

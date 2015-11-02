{-# LANGUAGE ExistentialQuantification #-}
module Eval where

import Types

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

-- function application
eval env (List (f : args)) = do
  func <- eval env f
  argVals <- mapM (eval env) args
  apply env func argVals

--
eval env val = return val

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

-- apply the function to its parameters
apply :: Env -> LispVal -> [LispVal] -> IOThrows LispVal
apply env (NativeFunc func) args = func env args
apply _ (Func params varargs body closure) args =
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
apply _ val args = throwError $ NotFunction "The value is not a function" $ show val

-- bound functions
nativeBindings :: IO Env
nativeBindings = newEnv >>= (flip bindVars $ map makeNativeFunc natives)
  where makeNativeFunc (name, func) = (name, NativeFunc func)

-- a list of native functions
natives :: [(String, Env -> [LispVal] -> IOThrows LispVal)]
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
          -- TODO make the things a function?
          -- ("cond", cond),
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
numericBinop :: (Integer -> Integer -> Integer) -> Env -> [LispVal] -> IOThrows LispVal
numericBinop op env []          = throwError $ NumArgs 2 []
numericBinop op env single@[_]  = throwError $ NumArgs 2 single
numericBinop op env params      = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> IOThrows a) -> (a -> a -> Bool) -> Env -> [LispVal] -> IOThrows LispVal
boolBinop unpacker op env args =  if length args /= 2
                                    then throwError $ NumArgs 2 args
                                    else do
                                      [l,r] <- mapM unpacker args
                                      return $ Bool $ l `op` r

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
stringBoolBinop = boolBinop unpackString

unpackNum :: LispVal -> IOThrows Integer
unpackNum (Number n) = return n
unpackNum notNumber = throwError $ TypeMismatch "number" notNumber

unpackBool :: LispVal -> IOThrows Bool
unpackBool (Bool t) = return t
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackString :: LispVal -> IOThrows String
unpackString (String t) = return t
unpackString notString = throwError $ TypeMismatch "string" notString

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> IOThrows a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> IOThrows Bool
unpackEquals v1 v2 (AnyUnpacker unp) =
    do
      u1 <- unp v1
      u2 <- unp v2
      return $ u1 == u2
  `catchError` (const $ return False)

isstring :: Env -> [LispVal] -> IOThrows LispVal
isstring _ [String _] = return $ Bool True
isstring _ l@(_:_) = throwError $ NumArgs 1 l
isstring _ _ = return $ Bool False

isnumber :: Env -> [LispVal] -> IOThrows LispVal
isnumber _ [Number _] = return $ Bool True
isnumber _ l@(_:_) = throwError $ NumArgs 1 l
isnumber _ _ = return $ Bool False

issymbol :: Env -> [LispVal] -> IOThrows LispVal
issymbol _ [Atom _] = return $ Bool True
issymbol _ l@(_:_) = throwError $ NumArgs 1 l
issymbol _ _ = return $ Bool False

isboolean :: Env -> [LispVal] -> IOThrows LispVal
isboolean _ [Bool _] = return $ Bool True
isboolean _ l@(_:_) = throwError $ NumArgs 1 l
isboolean _ _ = return $ Bool False

islist :: Env -> [LispVal] -> IOThrows LispVal
islist _ [List _] = return $ Bool True
islist _ l@(_:_) = throwError $ NumArgs 1 l
islist _ _ = return $ Bool False

ispair :: Env -> [LispVal] -> IOThrows LispVal
ispair _ (List (_:[]):_) = return $ Bool False
ispair _ (List (_:_):_) = return $ Bool True
ispair _ (DottedList [] _:_) = return $ Bool False
ispair _ (DottedList (_:_) _:_) = return $ Bool True
ispair _ _ = return $ Bool False

not' :: Env -> [LispVal] -> IOThrows LispVal
not' _ [Bool f] = return $ Bool $ not f
not' _ l@(_:_) = throwError $ NumArgs 1 l
not' _ _ = return $ Bool False

symbolToString :: Env -> [LispVal] -> IOThrows LispVal
symbolToString _ [Atom n] = return $ String n
symbolToString _ l@(_:_) = throwError $ NumArgs 1 l

car :: Env -> [LispVal] -> IOThrows LispVal
car _ [(List (x:xs))] = return x
car _ [(DottedList (x:xs) _)] = return x
car _ [arg] = throwError $ TypeMismatch "pair" arg
car _ args = throwError $ NumArgs 1 args

cdr :: Env -> [LispVal] -> IOThrows LispVal
cdr _ [(List (x:xs))] = return $ List xs
cdr _ [(DottedList [_] r)] = return r
cdr _ [(DottedList (_:xs) x)] = return $ DottedList xs x
cdr _ [arg] = throwError $ TypeMismatch "pair" arg
cdr _ args = throwError $ NumArgs 1 args

cons :: Env -> [LispVal] -> IOThrows LispVal
cons _ [x, List []] = return $ List [x]
cons _ [x, List ls] = return $ List (x:ls)
cons _ [x, DottedList ls tail] = return $ DottedList (x:ls) tail
cons _ [x, y] = return $ DottedList [x] y
cons _ args = throwError $ NumArgs 2 args

iseqv :: Env -> [LispVal] -> IOThrows LispVal
iseqv _ [(Bool l), (Bool r)] = return $ Bool (l == r)
iseqv _ [(Number l), (Number r)] = return $ Bool (l == r)
iseqv _ [(String l), (String r)] = return $ Bool (l == r)
iseqv _ [(Character l), (Character r)] = return $ Bool (l == r)
iseqv env [DottedList xs x, DottedList ys y] = iseqv env [List (xs ++ [x]),List (ys ++ [y])]
iseqv env [List xs, List ys] = do
  let zipped = zip xs ys
  eqvs <- mapM (\(x,y) -> iseqv env [x,y]) zipped
  return $ Bool $ (length xs == length ys) && all isTrue eqvs
  where isTrue (Bool True) = True
        isTrue _ = False
iseqv _ [_,_] = return $ Bool False
iseqv _ args = throwError $ NumArgs 2 args

isequal :: Env -> [LispVal] -> IOThrows LispVal
isequal env [v1, v2] = do
  primEquals <- liftM or $ mapM (unpackEquals v1 v2)
                [AnyUnpacker unpackNum, AnyUnpacker unpackString, AnyUnpacker unpackBool]
  (Bool x) <- iseqv env [v1, v2]
  return $ Bool $ (x || primEquals)

isequal _ args = throwError $ NumArgs 2 args

cond :: Env -> [LispVal] -> IOThrows LispVal
cond env ((List [test,rtn]):rest) = do
  result <- eval env test
  case result of
    Bool True -> eval env rtn
    Bool False -> cond env rest
    -- TODO throw error about booleans
    _ -> cond env rest
cond _ [] = throwError NonExhaustive

makestring :: Env -> [LispVal] -> IOThrows LispVal
makestring _ [Number n, Character c] = return $ String $ replicate (fromIntegral n) c
makestring env [n@(Number _)] =  makestring env [n, Character ' ']
makestring _ args = throwError $ NumArgs 2 args

string :: Env -> [LispVal] -> IOThrows LispVal
string _ [Character c] = return $ String [c]
string env ((Character c):cs) = do
  (String s) <- string env cs
  return $ String $ c:s
string _ badArgs = throwError $ ErrorList $ map (TypeMismatch "character") badArgs

stringlength :: Env -> [LispVal] -> IOThrows LispVal
stringlength _ [String s] = return $ Number $ toInteger $ length s
stringlength _ [arg] = throwError $ TypeMismatch "string" arg
stringlength _ args = throwError $ NumArgs 1 args

-- checks if n e [min,max)
withinBounds :: Int -> Int -> Int -> Bool
withinBounds min max n = n >= min && n < max

sublist :: Int -> Int -> [a] -> [a]
sublist min max l = take (max - min) $ drop min l

-- TODO proper error
stringref :: Env -> [LispVal] -> IOThrows LispVal
stringref _ [String s, Number ninteger] =
  let
    n = fromIntegral ninteger
  in
    if withinBounds 0 (length s) n
      then return $ Character $ s !! n
      else throwError $ IndexError n (length s)
stringref _ [f,s] = throwError $ ErrorList [TypeMismatch "string" f, TypeMismatch "number" s]
stringref _ args = throwError $ NumArgs 2 args

-- TODO  proper error
-- TODO fix <<loop>>
substring :: Env -> [LispVal] -> IOThrows LispVal
substring _ [Number st, Number en, String s] =
  let
    start = fromIntegral st
    end = fromIntegral end
    len = length s
  in
    if withinBounds 0 len start && withinBounds 0 len end && start < end
      then return $ String $ sublist start end s
      else throwError $ ErrorList [IndexError start len, IndexError end len]
substring _ [f,s,t] = throwError $ ErrorList [TypeMismatch "number" f, TypeMismatch "number" s, TypeMismatch "string" t]
substring _ args = throwError $ NumArgs 3 args

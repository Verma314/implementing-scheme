module Eval where

import Parser    
import ErrorCatcher
import Control.Monad.Except

-- this shall help us evaluate LispVals, and then return a LispVal
eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _) = return val -- this is not correct
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", pred, conseq, alt ] ) = do
                                               -- evaluate the predicate
                                               result <- eval pred  
                                               case result of 
                                                    Bool False -> eval alt
                                                    otherwise  -> eval conseq
eval (List [Atom "if", pred, Atom "then", conseq, Atom "else", alt ] ) = do
                                               -- evaluate the predicate
                                               result <- eval pred  
                                               case result of 
                                                    Bool False -> eval alt
                                                    otherwise  -> eval conseq
eval (List [Atom "quote", val]) = return val 
--  The last clause is our first introduction to nested patterns. 
-- The type of data contained by List is [LispVal], a list of LispVals. We match that against the specific two-element list [Atom "quote", val], a list where the first element is the symbol "quote" and the second element can be anything. Then we return that second element.
-- this belwo is for function evaluation, like (+ 1 2), which is LispVal terms is written like - [Atom "+",Number 1, Number 3]
-- we first evaluate the args too.
-- and then apply the operator to the result
eval (List ((Atom operator) : args)) =  mapM eval args >>= apply operator
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- examples: 
-- evaluation of eval (List ((Atom operator) : args)) 
-- > eval $ List [Atom "+", Number 2, Number 4]
--   Right 6
--
-- evaluation of the quoted element 
-- > eval $ List [Atom "quote", Bool True]
--   Right #t

{-------------------------------

maybe function works like this:
if we have a value wrapped in a context in the 3rd argument, it applies the function on that argument,
    otherwise it returns the 1st argument as a failsafe mechanism.

> maybe 999 (+ 1) (Just 20)
21
> maybe 999 (+ 1) (Nothing)
999

our apply function works, 
because what ever we get from (lookup operator primitives),
the maybe function applies the $ lispVals to it
something like: (removeContext (lookup operator primitives) $ lispVals)

--------------------------------}

-- new @ErrorHandling
apply :: String -> [LispVal] -> ThrowsError LispVal
apply operator lispVals = maybe (throwError $ NotFunction "Unrecognized primitive function args" operator)
                                ($ lispVals )
                                (lookup operator primitives)
                                
-- old @WithoutErrorHandling
--apply' :: String -> [LispVal] -> LispVal
--apply' operator lispVals = maybe (Bool False) ($ lispVals) (lookup operator primitives)



-- new @ErrorHandling
-- primitives' :: [(String, [LispVal] -> ThrowsError LispVal)]

-- old @WithoutErrorHandling
-- primitives is a Map/Dictionary
-- values of the pairs are functions from [LispVal] to LispVal
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [  ("+", numericBinop (+)),
                ("-", numericBinop (-)),
                ("*", numericBinop (*)),
                ("/", numericBinop div),
                ("mod", numericBinop mod),
                ("quotient", numericBinop quot),
                ("remainder", numericBinop rem),
                ("=", numBoolBinop (==)),
                ("<", numBoolBinop (<)),
                (">", numBoolBinop (>)),
                ("/=", numBoolBinop (/=)),
                (">=", numBoolBinop (>=)),
                ("<=", numBoolBinop (<=)),
                ("&&", boolBoolBinop (&&)),
                ("||", boolBoolBinop (||)),
                ("string=?", strBoolBinop (==)),
                ("string<?", strBoolBinop (<)),
                ("string>?", strBoolBinop (>)),
                ("string<=?", strBoolBinop (<=)),
                ("string>=?", strBoolBinop (>=))
              ]




numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do
                                  -- unpack the args, (unpack) the 1st argument, and the 2nd argument
                                  -- apply the operator op on these values
                                  left <- unpacker $ args !! 0 
                                  right <- unpacker $ args !! 1
                                  return ( Bool $ left `op` right)

-- now using this general boolBinop,
-- we can use it with different kinds of unpackers to create diffent types of boolBinOp (or BOOL BINary OPerator) functions
-- likes ones in which the input is a number, ones in which the input is a string, etc.
-- we have an unpackNum which can take LispVals and convert them into native haskell types.
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool






-- old @WithoutErrorHandling
-- numericBinop takes a primitive function (like +,-,etc)
-- and wraps it with code to unpack an argument list, 
-- apply the function to it, and wrap the result up in our Number constructor
--numericBinop' :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
--numericBinop' op params = Number $ foldl1 op  $ map unpackNum params

{-------------
How does parsed work?
> parsed "12345"
[(12345,"")]
-----------}

-- takes a LispVal, and returns an Integer (wrapped in a context)
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum






{-
unpackNum' :: LispVal -> Integer
unpackNum' (Number n) = n
unpackNum' (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0 
unpackNum' (List [n]) = unpackNum n
unpackNum' _ = 0  

-}




unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return ( show s)
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool










{-
Summary:


Now we can do this,
> eval (List [Atom "+", Number 1, Number 3, String "42345"])
42349

AND also this,

> mainGhci "(+ 1 2)"
3
> mainGhci "(+ 1 2 3)"
6
> 
> mainGhci "(+ 1 2 3 (* 4 5))"
26






-}

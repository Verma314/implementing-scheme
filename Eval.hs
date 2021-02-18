module Eval where

import Parser    
import ErrorCatcher


-- this shall help us evaluate LispVals, and then return a LispVal
eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _) = return val -- this is not correct
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val --  The last clause is our first introduction to nested patterns. The type of data contained by List is [LispVal], a list of LispVals. We match that against the specific two-element list [Atom "quote", val], a list where the first element is the symbol "quote" and the second element can be anything. Then we return that second element.
-- this belwo is for function evaluation, like (+ 1 2), which is LispVal terms is written like - [Atom "+",Number 1, Number 3]
-- we first evaluate the args too.
-- and then apply the operator to the result
eval (List ((Atom operator) : args)) =  mapM eval args >>= apply operator
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


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

apply :: String -> [LispVal] -> ThrowsError LispVal
apply operator lispVals = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                                ($ lispVals )
                                (lookup operator primitives)
                                

apply' :: String -> [LispVal] -> LispVal
apply' operator lispVals = maybe (Bool False) ($ lispVals) (lookup operator primitives)


-- primitives is a Map/Dictionary
-- values of the pairs are functions from [LispVal] to LispVal
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]


-- numericBinop takes a primitive function (like +,-,etc)
-- and wraps it with code to unpack an argument list, 
-- apply the function to it, and wrap the result up in our Number constructor
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op  $ map unpackNum params

{-------------
How does parsed work?
> parsed "12345"
[(12345,"")]
-----------}
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0 
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0                               
{-

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
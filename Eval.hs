module Eval where

import Parser    


-- this shall help us evaluate LispVals, and then return a LispVal
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val


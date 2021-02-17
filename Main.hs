module Main where  

import Parser
import ParserReadExpr

import Eval
import ErrorCatcher

import Control.Monad
import System.Environment

-- Notes:
-- 1. We deprecated readExpr 
-- 

-- getArgs does not work inside ghci
main :: IO ()
main = getArgs >>= print . eval . readExpr2 . head

mainGhci :: String -> IO ()
mainGhci inputExpr = do 
                     (print . eval. readExpr2 ) inputExpr

repl :: IO ()
repl = do 
       inputStrs <- getLine
       (print . eval . readExpr2 )  inputStrs
       repl

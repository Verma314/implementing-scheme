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


main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr2 (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled

-- getArgs does not work inside ghci
main' :: IO ()
main' = getArgs >>= print . eval . readExpr . head


{-
mainGhci :: String -> IO ()
mainGhci inputExpr = do 
                     (print . eval. readExpr ) inputExpr
-}

repl :: IO ()
repl = do 
       inputStrs <- getLine
       evaled <- return $ liftM  show $ readExpr2   (inputStrs ) >>= eval
       putStrLn $ extractValue $ trapError evaled
       repl

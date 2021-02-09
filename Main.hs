import Parser
import Eval

import Control.Monad
import System.Environment

-- getArgs does not work inside ghci
main :: IO ()
main = getArgs >>= print . eval . readExpr . head

mainGhci :: String -> IO ()
mainGhci inputExpr = do 
                     (print . eval. readExpr ) inputExpr

repl :: IO ()
repl = do 
       inputStrs <- getLine
       (print . eval . readExpr )  inputStrs
       repl

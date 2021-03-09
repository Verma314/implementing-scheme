module Main where  

import Parser
import ParserReadExpr

import Eval
import ErrorCatcher

import Control.Monad
import System.Environment
import System.IO

import Runtime
-- Notes:
-- 1. We deprecated readExpr 
-- 

{-
-- old
main2' :: IO ()
main2' = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr2 (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled

-- getArgs does not work inside ghci
main' :: IO ()
main' = getArgs >>= print . eval . readExpr . head

-}

{-
mainGhci :: String -> IO ()
mainGhci inputExpr = do 
                     (print . eval. readExpr ) inputExpr


repl_old :: IO ()
repl_old = do 
       putStr " > "
       inputStrs <- getLine
       evaled <- return $ liftM  show ( readExpr2  inputStrs >>= eval)
       putStrLn $ extractValue $ trapError evaled
       repl
       -- above readExpr2  inputStrs generates a 'ThrowsError LispVal'
       -- the LispVal is piped into the eval,
       -- which returns a "Either LispError LispVal"
       -- after which it is converted to a string.
       -- then put the value into an IO context.

-}
---------------------------------------------------------
------------------ Building a repl ----------------------
---------------------------------------------------------

-- in my opinion, this repl is overkill and no much better than
-- the previous repl that I had implemented.
-- i.e. repl_old


-- helper functions:
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout
-- prints out a string.
-- hFlush stdout flushes the stream. 
-- ie it ensures everything in buffer is printed out


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
-- prints out a prompt, and reads in a line of input.

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr2 expr) >>= eval env


--evalString' :: String -> IO String
--evalString' expr = return $ extractValue $ trapError (liftM show $ readExpr2 expr >>= eval)
       -- above readExpr2  inputStrs generates a 'ThrowsError LispVal'
       -- the LispVal is piped into the eval,
       -- which returns a "Either LispError LispVal"
       -- after error handling, it is put back in the IO context (using return)


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

--evalAndPrint' :: String -> IO ()
--evalAndPrint' expr =  evalString expr >>= putStrLn
-- a function that evaluates a string and prints the result:       


-- predicate : is the condition when we quit the until_ loop
-- promt : is the input (here, an IO String) to our replFunction
-- action : is the main evaluation function (ie the replFunction) for our language, which takes in the inputs (ie prompt)
-- the action function keeps getting executed, unless the predicate becomes true.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
          -- get the input out of the context, this is the input args
          result <- prompt
          -- check to see if it is same as the predicate (== "quit")
          if predicate result  
          then  return ()
          -- else call the action
          else action result >> until_ predicate prompt action   

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "> ") . evalAndPrint

--runRepl :: IO ()
--runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

repl = runRepl

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"


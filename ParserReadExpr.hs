module ParserReadExpr where  

import Control.Monad
import Control.Monad.Except

import Parser
import ErrorCatcher
import Text.ParserCombinators.Parsec hiding (spaces)

-- @ErrorHandling
readExpr2 :: String -> ThrowsError LispVal -- same as : Either LispError LispVal
readExpr2 input = case parse parseExpr "lisp" input of
    Left err ->   throwError ( Parser err )
    -- here above we wanna return "Left LispError", 
    -- so something like return (Parser err), or Left (Parser err) would also work. 
    -- We use throwError (as it can also put an error value into a Left data constructor) 
    Right val ->  return (val ) 
    -- why use return? 
    -- to put it back into the Either context (remember that ThrowsError is actually an Either type synonym)


-- old @WithoutErrorHandling
readExpr2' :: String -> LispVal
readExpr2' input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err -- this err, actually is of type ParseError and belongs to the module Text.ParserCombinators.Parsec
    Right val ->  val 
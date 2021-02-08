module Parser where                              

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


----------------------------------
------ a parser for symbols ------
----------------------------------

-- we'll define a parser that recognizes one of the symbols allowed in Scheme identifiers:
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


readExpr_ :: String -> String
readExpr_ input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


-------------------------------------------
--- a data type to hold any lisp values ---
-------------------------------------------

-- this is the target state, we wanna parse our input strings into one of these
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- representing the Scheme form (a b . c); also called an improper list. This stores a list of all elements but the last, and then stores the last element as another field
             | Number Integer
             | String String
             | Bool Bool -- deriving (Show)



-- this is so that _after_ we have our target LispVal, we can convert them back into nice syntax
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List lispExprList) = "(" ++ unwordsList lispExprList ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

--helper fxn:
unwordsList :: [LispVal] -> String
unwordsList listOfLispExprs = unwords (map showVal listOfLispExprs)
{-
> x = [Atom "a", Atom "b", Atom "c"]
>
> unwordsList x
"a b c"
-}

-- instantiate show for LispVal type, so that when parser recognizes the values, 
   -- it doesn't show their representation in weird haskell syntax.
       -- it prints out the values in beautiful LISP-like syntax
instance Show LispVal where show = showVal




-----------------------------------
----- Parsing Scheme Strings ------
-----------------------------------

-- we don't want the parser just to "accept/not-accept" an input, 
-- we want it to convert to proper data type values that haskell can understand
-- how we parse a String into a LispVal
parseString :: Parser LispVal -- LispVal in a Parser Context
parseString = do 
              char '"'
              x <- many (noneOf "\"") -- can be anything except ```"```
              char '"'
              return $ String x
-- in the last line ^ We  apply the built-in function return to lift our LispVal into the Parser monad.                 



-----------------------------------
-- Parsing Scheme Variables/Atom --
-----------------------------------

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol -- first character should be a letter or symbol
              rest <- many (letter <|> digit <|> symbol) -- followed by many letters or symbols or digits
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

--------------------------------
---- Parsing Scheme numbers ----
--------------------------------

parseNumber :: Parser LispVal
parseNumber = liftM (Number .read ) $ many1 digit

--------------------------------------------------------------------
---- A combined parser for that accepts Strings, Numbers, Atoms ----
--------------------------------------------------------------------

parseExpr_old :: Parser LispVal
parseExpr_old = parseString <|> parseNumber <|> parseAtom


-------------------
-- parsing lists --
-------------------
parseList :: Parser LispVal
parseList =  liftM List $ sepBy parseExpr spaces

------------------------------------
-- parsing dotted list -- a b . c --
------------------------------------
parseDottedList :: Parser LispVal
parseDottedList = do 
                  head <- endBy parseExpr spaces
                  tail <- char '.' >> spaces >> parseExpr
                  return $ DottedList head tail  


--------------------------------------------
-- parsing quoted values list example, 'a --
--------------------------------------------
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


---------------------------------------------
----- Putting our mini parsers together -----
---------------------------------------------

-- the key function that parses our strings, classifies them implicity, and returns a ``` Parser LispVal```
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|>  parseDottedList 
                char ')'
                return x

readExpr_old' :: String -> String
readExpr_old' input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

readExpr_old'' input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val -- no need to a "show" val as well, we can return the actual LispVal

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val ->  val 


{-
main_old :: IO ()
main_old = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)

-}
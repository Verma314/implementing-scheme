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

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- representing the Scheme form (a b . c); also called an improper list. This stores a list of all elements but the last, and then stores the last element as another field
             | Number Integer
             | String String
             | Bool Bool deriving (Show)



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

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|>  parseDottedList 
                char ')'
                return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"


main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)



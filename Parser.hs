import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


-- this ```symbol``` _is_ a parser
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

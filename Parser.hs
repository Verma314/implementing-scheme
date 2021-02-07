import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


-- we'll define a parser that recognizes one of the symbols allowed in Scheme identifiers:
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

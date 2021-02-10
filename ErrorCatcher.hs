module ErrorCatcher where  

import Control.Monad.Except
import Parser
import Text.ParserCombinators.Parsec hiding (spaces) -- why do we have to import it multiple times? todo: prevent importing it multiple times, 2nd import in Parse module

-- we need a data type to represent an error
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError -- fom the Parsec library.
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found                                       
showError (Parser parseErr)             = "Parse error at " ++ show parseErr -- one that we will use the most probably for now


instance Show LispError where show = showError
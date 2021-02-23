module ErrorCatcher where  

import Control.Monad.Except
import Parser
import Text.ParserCombinators.Parsec hiding (spaces) -- why do we have to import it multiple times? todo: prevent importing it multiple times, 2nd import in Parse module

-- we need a data type to represent an error
data LispError = NumArgs Integer [LispVal] -- incorrect number of arguments
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




-- Type constructors are curried just like functions, and can also be partially applied. 
-- so we create a new type, it's an Either type
-- ƒor the Left, it would return a LispError,
-- and the right can be anything (for now), so we define it like:
type ThrowsError = Either LispError
-- in the future,
    -- for the type say "Either LispError LispVal", we would use type "ThrowsError LispVal"

{-
The Control.Monad.Except library automatically gives the Either monad
two other functions (besides the standard monadic ones):
1. throwError, which takes an Error value and lifts it into the Left (error) constructor of an Either
2. catchError, which takes an Either action and a function 
        that turns an error into another Either action. 
    If the action represents an error, it applies the function, which you can use:
    e.g. turn the error value into a normal one via return or re-throw as a different error.
-}

-- we shall convert all our errors into string representations
trapError action = catchError action (return . show)
-- this fxn accepts a Monadic action (one that returns an Either a b in our case)
-- after which in case 'action' returns a value wrapped in the 'Left' data constructor
-- ... we convert the error into a String, we wrap it inside the Mondad (here, 'Right' done via return)
-- hence this fxn never throws an error, (i.e if poassed Either, it always returns a Right)
-- although it still "represents" an error

-- we need a fxn to extract out the val from inside the 'Right'
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- we will now introduce error handling into our existing code
-- the new function versions are tagged -- @ErrorHandling 
-- and the older ones are tagged -- @WithoutErrorHandling


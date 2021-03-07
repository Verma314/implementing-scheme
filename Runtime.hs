module Runtime where   

import Data.IORef
import Parser
import ErrorCatcher
import Control.Monad.Except

-- IORef (like the ST Monad) is a 'state thread'
-- these let you do stateful computations that can be executed as a unit, without the state escaping to the rest of the progr


-- defined a type for our environment
type Env = IORef [(String, IORef LispVal)]
-- this is a map that maps string to mutable LispVals. Which means the value of a variable can change.
-- plus, this map itself can change (ie mutable). Which means new varibles can be added


{-
Note,

newIORef is a function, that accepts an argument 
and wraps it in IORef (i.e makes it mutable),
and then wraps the whole thing in IO.

Why IO?

Because IORefs can only be used within the IO monad.

> :t newIORef
newIORef :: a -> IO (IORef a)


-}

nullEnv :: IO Env
nullEnv = newIORef []


-- we use a monad transformer ExceptT,
-- monad transformers let us combine the functionality of two monads.
-- ExceptT lets us layer error-handling functionality on top of the IO monad
-- So create a type synonym for our combined monad:

type IOThrowsError = ExceptT LispError IO
-- ExceptT takes one more argument, the return type of the function.

-- helper functions
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
--



--------------------------------------------------
-- methods for handling the runtime environment --
--------------------------------------------------


-- determine if a given variable is already bound in the environment
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var
-- ```readIORef envRef``` returns a map in the IO context. and pipes it to the 2nd statment, 
-- which tells us if the variable is present in the environment/map.




-- a function to retrieve the current value of a variable:
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)
-- readIORef takes the IORef into the IO context.
-- liftIO takes values from IO Context, and into the IOThrowsError context 


-- a function to set values:







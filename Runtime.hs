module Runtime where   

import Data.IORef

-- IORef (like the ST Monad) is a 'state thread'
-- these let you do stateful computations that can be executed as a unit, without the state escaping to the rest of the progr


-- defined a type for our environment
type Env = IORef [(String, IORef LispVal)]

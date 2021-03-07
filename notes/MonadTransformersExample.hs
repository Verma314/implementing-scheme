import Control.Monad
import Control.Applicative
import Control.Monad.Trans

import Data.Char

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- make ```MaybeT m``` an instance of Monad type

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return. Just

    -- The signature of (>>=), specialized to MaybeT m:
    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    x >>= f = MaybeT $ do maybeValue <- runMaybeT x
                          case maybeValue of 
                              Nothing -> return Nothing
                              (Just val) -> runMaybeT $ f val
                          

-- everything that is a monad should also be a functor, and an applicative
instance (Monad m) => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap 

instance (Monad m) => Functor (MaybeT m) where
    fmap = liftM
-- see https://stackoverflow.com/questions/32929252/can-ghc-derive-functor-and-applicative-instances-for-a-monad-transformer


instance Monad m => Alternative (MaybeT m) where
    empty   = MaybeT $ return Nothing
    x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing    -> runMaybeT y
                               Just _     -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where 
    mzero = empty
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)
 
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

test :: MaybeT IO String
test = do 
       x <- lift getLine
       let y = x ++ "no"
       return (y)

test2 ::  [ Int]
test2 = do 
       x <- [ 10,11,12]
       let k = x + 1 
       return k
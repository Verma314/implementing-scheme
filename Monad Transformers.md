
# Monad Transformers

(from the resource : https://en.wikibooks.org/wiki/Haskell/Monad_transformers)

Monad transformers lets us use the capabilities of several monads into one.

Like composing two monads into a single monad that shares the behaviour of both. 

## MaybeT

"We will define a monad transformer that gives the IO monad some characteristics of the Maybe monad; we will call it ```MaybeT```.

```MaybeT``` is just a *wrapper* around ```m (Maybe a)```, where ```m``` can be any monad (```IO``` in our example).

```
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```





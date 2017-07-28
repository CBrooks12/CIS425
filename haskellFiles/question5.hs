import Control.Applicative

data StateM a = StateM (Integer -> (a, Integer))

get :: StateM Integer
get = StateM (\state -> (state, state))

put :: Integer -> StateM ()
put newState = StateM (\oldState -> ((), newState))

instance Monad StateM where
  -- return :: a -> StateM a
  return x = StateM (\state -> (x, state))

  -- (>>=) :: StateM a -> (a -> StateM b) -> StateM b
  (StateM f) >>= k = StateM (\state ->
    let
      (x, newState) = f state
      StateM f' = k x
    in f' newState)

{- we need this last part to work with new versions of GHC -}
instance Functor StateM where
   fmap f m = m >>= (\x -> return (f x))

instance Applicative StateM where
   pure = return
   mf <*> mx = do f <- mf
                  x <- mx
                  return (f x)
        
increment :: StateM ()
increment = do
    x <- get
    put (x + 1)
    
run :: StateM () -> Integer
run (StateM f) = snd(f 0)
    
    


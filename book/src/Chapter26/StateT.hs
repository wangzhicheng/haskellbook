module Chapter26.StateT where

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f (StateT smas) = StateT (\s -> fmap g (smas s))
        where
            g (a, s) = (f a, s)

instance Monad m => Applicative (StateT s m) where
    pure a = StateT (\s -> pure (a, s))
    StateT smas <*> StateT smas' = StateT $ \s
        -> do
               (a, s1) <- smas s
               (a', s2) <- smas' s1
               return (a a', s2)

instance Monad m => Monad (StateT s m) where
    return = pure
    StateT smas >>= f = StateT $ \s -> do
        (a, s1) <- smas s
        runStateT (f a) s1


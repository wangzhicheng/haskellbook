{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Chapter23.State where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi r) = Moi $ (\(a, s) -> (f a, s)) . r

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi (a,)

    (<*>) :: Moi s (a -> a') -> Moi s a -> Moi s a'
    Moi f <*> Moi f' = Moi (\s -> (fst (f s) $ fst (f' s) , s))

instance Monad (Moi s) where
    return :: a -> Moi s a
    return = pure

    (>>=) :: forall s a b. Moi s a -> (a -> Moi s b) -> Moi s b
    Moi r >>= f = Moi $ f' . r
        where
            f' :: (a, s) -> (b, s)
            f' (a, s) = runMoi (f a) s

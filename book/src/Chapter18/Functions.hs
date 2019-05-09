module Chapter18.Functions where

import           Control.Monad

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do
    x <- ma
    f <- mf
    return $ f x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _  =  return []
meh s f   =  go s f
    where go (x:xs) g  =  do
            b <- g x
            bs <- go xs g
            return $ b : bs

flipType :: Monad m => [m a] -> m [a]
flipType l = meh l id

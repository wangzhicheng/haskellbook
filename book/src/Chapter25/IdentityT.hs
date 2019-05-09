{-# LANGUAGE InstanceSigs #-}

module Chapter25.IdentityT where

newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT ma) = IdentityT (f <$> ma)

instance Applicative Identity where
    pure = Identity
    Identity a <*> Identity a' = Identity (a a')

instance Applicative m => Applicative (IdentityT m) where
    pure = IdentityT . pure
    IdentityT ff <*> IdentityT fa = IdentityT (ff <*> fa)

instance Monad Identity where
    return = pure
    Identity a >>= f = f a

instance Monad m => Monad (IdentityT m) where
    return = pure
    (>>=) :: IdentityT m a
          -> (a -> IdentityT m b)
          -> IdentityT m b
    IdentityT ma >>= f = IdentityT $ ma >>= runIdentityT . f


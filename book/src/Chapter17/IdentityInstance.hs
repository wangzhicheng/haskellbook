module Chapter17.IdentityInstance where

newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a)

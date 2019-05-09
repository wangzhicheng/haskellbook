{-# LANGUAGE ExplicitForAll #-}
module Chapter05.Exparametricity where

identity :: forall a. a -> a
identity x = x

f :: forall a. a -> a -> a
f x _ = x

f' :: forall a. a -> a -> a
f' _ y = y

g :: forall a. a -> b -> b
g _ y = y

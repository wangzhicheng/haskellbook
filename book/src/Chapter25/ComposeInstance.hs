{-# LANGUAGE InstanceSigs #-}

module Chapter25.ComposeInstance where

import           Data.Foldable (fold)

newtype Compose f g a = Compose {getCompose :: f (g a)}
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    Compose fga <*> Compose fga' = Compose $ (<*>) <$> fga <*> fga'

instance (Functor f, Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = fold $ foldMap f <$> fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative f'
             => (a -> f' b)
             -> Compose f g a
             -> f' (Compose f g b)
    traverse f cfga = sequenceA $ f <$> cfga

    sequenceA :: Applicative h
              => Compose f g (h a)
              -> h (Compose f g a)
    sequenceA (Compose fgha) = Compose <$> (sequenceA . (sequenceA <$>)) fgha


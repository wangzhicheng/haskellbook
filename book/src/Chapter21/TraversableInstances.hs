module Chapter21.TraversableInstances where

import           Prelude hiding (Either (..))

-----------------------------------------------------------------------------
data Either a b = Left a | Right b
    deriving (Eq, Ord, Show)

instance Functor (Either a) where
    fmap _ (Left a)  =  Left a
    fmap f (Right b) =  Right (f b)

instance Applicative (Either a) where
    pure           =  Right
    Left a <*> _   =  Left a
    Right b <*> r  =  fmap b r

instance Foldable (Either a) where
    foldMap _ (Left _)  =  mempty
    foldMap f (Right b) =  f b

    foldr _ z (Left _)  =  z
    foldr f z (Right b) =  f b z

instance Traversable (Either a) where
    traverse _ (Left x)  = pure (Left x)
    traverse f (Right y) = Right <$> f y

-----------------------------------------------------------------------------
data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    Two a b <*> Two a' b' = Two (a <> a') (b b')

instance Foldable (Two a) where
    foldMap f (Two _ b) = f b
    foldr f z (Two _ b) = f b z

instance Traversable (Two a) where
    traverse f (Two a b) = Two a <$> f b

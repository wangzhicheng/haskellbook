module Chapter21.TreeTraversable where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty          = Empty
    fmap f (Leaf a)       = Leaf (f a)
    fmap f (Node lt a rt) = Node (f <$> lt) (f a) (f <$> rt)

instance Foldable Tree where
    foldMap _ Empty          = mempty
    foldMap f (Leaf a)       = f a
    foldMap f (Node lt a rt) = foldMap f lt <> f a <> foldMap f rt

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node lt a rt) = Node <$> traverse f lt <*> f a <*> traverse f rt

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = oneof [ return Empty
                      , Leaf <$> arbitrary
                      , Node <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

main :: IO ()
main = quickBatch $ traversable (undefined :: Tree (Int, Int, String))

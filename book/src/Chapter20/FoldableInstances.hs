module Chapter20.FoldableInstances where

newtype Constant a b = Constant b

instance Foldable (Constant a) where
    foldr f x (Constant b) = f b x

data Two a b = Two a b

instance Foldable (Two a) where
    foldr f x (Two _ b) = f b x

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldr f x (Three _ _ c) = f c x

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldr f x (Four' _ b1 b2 b3) = foldr f x [b1, b2, b3]

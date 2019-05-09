
module Chapter22.ReaderPractice where

import           Control.Applicative (liftA2)
import           Data.Traversable    (sequenceA)
import           Prelude             hiding (lookup, uncurry)

x = [1, 2, 3]
y = [4 ,5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ []    = Nothing
lookup k (h:t) = if k == fst h then Just (snd h) else lookup k t

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (\x -> (x, x)) . z'

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt n = n > 3 && n < 8

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequenceA $ Just <$> [3, 2, 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(>3), (<8), even] 7
    print $ sequA $ fromMaybe 0 s'
    print $ bolt $ fromMaybe 0 ys

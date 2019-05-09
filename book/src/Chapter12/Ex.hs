module Chapter12.Ex where

-- import Data.Maybe (fromMaybe)
import           Data.Maybe (isNothing)

----------------------------------------------------------------------------------------------
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str   = Just str

replaceThe :: String -> String
replaceThe = foldr f "" . fmap (fromMaybe "a" . notThe) . words
    where
        f l r = l ++ " " ++ r

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel s = go (words s)
    where
        go []  = 0
        go strs
          | head strs /= "the"                          =  go (tail strs)
          | (head . head . tail $ strs) `elem` "aeiou"  =  1 + go (tail . tail $ strs)
          | otherwise                                   =  go (tail . tail $ strs)


----------------------------------------------------------------------------------------------
countVowels :: String -> Int
countVowels = length . filter isVowel
    where
        isVowel = flip elem "aeiou"

----------------------------------------------------------------------------------------------
newtype Word' = Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str
  | ((*2) . length . filter (`elem` vowels)) str > length str  =  Nothing
  | otherwise                                                  =  Just $ Word' str

----------------------------------------------------------------------------------------------
data Nat = Zero | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     =  0
natToInteger (Succ n) =  1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0      =  Nothing
  | n == 0     =  Just Zero
  | otherwise  =  Just . Succ . fromMaybe Zero . integerToNat $ n - 1

----------------------------------------------------------------------------------------------
isJust :: Maybe a -> Bool
isJust Nothing =  False
isJust _       =  True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  =  b
mayybee _ f (Just a) =  f  a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  =  a
fromMaybe _ (Just a) =  a

listToMaybe :: [a] -> Maybe a
listToMaybe []    =  Nothing
listToMaybe (x:_) =  Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  =  []
maybeToList (Just a) =  [a]

catMaybes :: [Maybe a] -> [a]
catMaybes  = fmap f . filter isJust
    where
        f :: Maybe a -> a
        f (Just a)  =  a
        f Nothing   =  error "f: Nothing supplied as an argument. This shouldn't have happened. Oops."

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms
  | (and . fmap isJust) ms  =  Just $ catMaybes ms
  | otherwise               =  Nothing

----------------------------------------------------------------------------------------------
left' :: [Either a b] -> [a]
left' = fmap deLeft . filter isLeft
    where
        isLeft :: Either a b -> Bool
        isLeft (Left _) =  True
        isLeft _        =  False
        deLeft :: Either a b -> a
        deLeft (Left a)  =  a
        deLeft _         =  error "`Right` supplied as an argument. This shouldn't have happened."

right' :: [Either a b] -> [b]
right' = fmap deRight . filter isRight
    where
        isRight :: Either a b -> Bool
        isRight (Right _) =  True
        isRight _         =  False
        deRight :: Either a b -> b
        deRight (Right b)  =  b
        deRight _          =  error "`Left` supplied as an argument. This shouldn't have happened."

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (left' es, right' es)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _)  =  Nothing
eitherMaybe' f (Right b) =  Just (f b)

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a)  =  f a
either' _ g (Right b) =  g b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' g = either' (const Nothing) (Just . g)

----------------------------------------------------------------------------------------------
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f v
  | isNothing (f v) =  []
  | otherwise       =  x : myUnfoldr f y
    where
        x = fst . g . f $ v
        y = snd . g . f $ v
        g :: Maybe (a, b) -> (a, b)
        g (Just (m, n)) = (m, n)
        g Nothing = error "`Nothing` suppled as an argument to `g`, This shouldn't have happened"

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

----------------------------------------------------------------------------------------------
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold f s = case f s of
               Nothing          -> Leaf
               Just (ls, n, rs) -> Node (unfold f ls) n (unfold f rs)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeBuilder 0
  where
    treeBuilder a
      | a < n     = Just (a + 1, a, a + 1)
      | otherwise = Nothing

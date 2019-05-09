module Chapter11.BinaryTree
    ( BinaryTree
    , insert'
    , mapTree
    , preorder
    , inorder
    , postorder) where

import           Data.List (sort)

------------------------------------------------------------------
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

------------------------------------------------------------------
insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
insert' v Leaf = Node Leaf v Leaf
insert' v (Node l n r)
          | v == n = Node l n r
          | v > n  = Node l n (insert' v r)
          | v < n  = Node (insert' v l) n r

------------------------------------------------------------------
mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf         = Leaf
mapTree f (Node l a r) = Node (mapTree f l) (f a) (mapTree f r)

------------------------------------------------------------------
testMapTree :: IO ()
testMapTree =
    let
        testTree' :: BinaryTree Integer
        testTree' =
            Node (Node Leaf 3 Leaf)
                1
                (Node Leaf 4 Leaf)
        expectedTree' :: BinaryTree Integer
        expectedTree' =
            Node (Node Leaf 4 Leaf)
                2
                (Node Leaf 5 Leaf)
    in
        if mapTree (+1) testTree' == expectedTree'
           then print "yup okay!"
           else error "test failed!"

------------------------------------------------------------------
preorder :: BinaryTree a -> [a]
preorder Leaf         = []
preorder (Node l a r) = a : preorder l  ++ preorder r

------------------------------------------------------------------
inorder :: Ord a => BinaryTree a -> [a]
inorder = sort . preorder

------------------------------------------------------------------
postorder :: Ord a => BinaryTree a -> [a]
postorder = reverse . inorder

------------------------------------------------------------------
testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
         2
         (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
       then putStrLn "Preorder fine!"
       else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
       then putStrLn "Inorder fine!"
       else putStrLn "Bad news bears."

------------------------------------------------------------------

main :: IO ()
main = do
    testMapTree
    testPreorder
    testInorder

-- count the number of leaves of a tree
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

countLeaves :: Tree Int -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves t@(Branch _ l r) = countLeaves l + countLeaves r

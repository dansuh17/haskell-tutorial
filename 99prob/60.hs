-- construct height-balanced binary trees
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

-- filter out by height difference
-- construct height-balanced tree for only given number of nodes
hbalTreeNodes :: Int -> [Tree Char]
hbalTreeNodes 0 = [Empty]
hbalTreeNodes n = filter isHeightBalanced (allTrees n)

isHeightBalanced :: Tree Char -> Bool
isHeightBalanced Empty = True
isHeightBalanced (Branch _ Empty r) = height r <= 1
isHeightBalanced (Branch _ l Empty) = height l <= 1
isHeightBalanced (Branch _ l r) = abs (height l - height r) <= 1 && isHeightBalanced l && isHeightBalanced r

height :: Tree Char -> Int
height Empty = 0
height t@(Branch _ l r) = 1 + maximum([height l, height r])

allTrees :: Int -> [Tree Char]
allTrees 0 = [Empty]
allTrees n = [Branch 'x' l r | i <- [0 .. (n - 1)], l <- allTrees i, r <- allTrees (n - 1 - i)]

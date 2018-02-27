-- generate and test paradigm
-- generate all symmetric and completely balanced binary trees given number of nodes
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

leaf a = Branch a Empty Empty

allTrees :: Int -> [Tree Char]
allTrees 0 = [Empty]
allTrees 1 = [leaf 'x']
allTrees n = [Branch 'x' l r | i <- [0 .. n - 1], l <- allTrees i, r <- allTrees (n - 1 - i)]

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric t@(Branch x l r) = isMirror l r

isMirror :: Tree a -> Tree a -> Bool
isMirror Empty Empty = True
isMirror (Branch _ a b) (Branch _ x y) = isMirror a y && isMirror b x
isMirror _ _ = False

balanced :: Tree a -> Bool
balanced Empty = True
balanced t@(Branch _ l r) = (height l) - (height r) <= 1

height :: Tree a -> Int
height Empty = 0
height t@(Branch _ l r) = 1 + maximum([height l, height r])

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter (\t -> symmetric t && balanced t) . allTrees

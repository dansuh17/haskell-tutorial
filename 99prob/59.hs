-- construct height-balanced binary trees
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

-- filter out by height difference from all possible number of nodes
-- the maximum possible number of nodes of maximum height n is 2 ^ n - 1
hbalTree :: Int -> [Tree Char]
hbalTree 0 = [Empty]
hbalTree n = filter (\t -> heightDiff t <= 1) $ concat [allTrees n | n <- [0 .. 2 ^ n - 1]]

heightDiff :: Tree Char -> Int
heightDiff Empty = 0
heightDiff t@(Branch _ l r) = abs (height l - height r)

height :: Tree Char -> Int
height Empty = 0
height t@(Branch _ l r) = 1 + maximum([height l, height r])

allTrees :: Int -> [Tree Char]
allTrees 0 = [Empty]
allTrees n = [Branch 'x' l r | i <- [0 .. (n - 1)], l <- allTrees i, r <- allTrees (n - 1 - i)]

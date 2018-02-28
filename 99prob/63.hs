-- construct complete binary tree
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show)

leaf x = Branch x Empty Empty

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = treeIdx 0 n

-- helper function - it constructs a tree by passing indices of the root nodes of subtree
treeIdx :: Int -> Int -> Tree Char
treeIdx i n
 | i < n = Branch 'x' (treeIdx (leftChildIdx i) n) (treeIdx (rightChildIdx i) n)
 | otherwise = Empty
  where leftChildIdx i = 2 * i + 1
        rightChildIdx i = 2 * (i + 1)

-- tests whether the tree is a complete binary tree
-- this simply creates a complete binary tree having same nodes as the provided tree
-- and tests whether both are identical.
isCompleteBinaryTree :: Tree Char -> Bool
isCompleteBinaryTree t = isTreeIdentical t (completeBinaryTree $ countNodes t)

-- count the nodes in a tree
countNodes :: Tree Char -> Int
countNodes Empty = 0
countNodes t@(Branch _ l r) = 1 + countNodes l + countNodes r

-- test whether two trees are exactly identical
isTreeIdentical :: Tree Char -> Tree Char -> Bool
isTreeIdentical Empty Empty = True
isTreeIdentical (Branch _ a b) (Branch _ x y)  = (isTreeIdentical a x) && (isTreeIdentical b y)
isTreeIdentical _ _ = False

-- isCompleteBinaryTree $ Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty) == True
-- isCompleteBinaryTree $ Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty) == False

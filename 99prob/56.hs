-- symmetric binary tree
-- a binary tree is symmetric if the left and right subtree are mirror images of each other

-- define the tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a)

-- determine whether two trees are mirrors of each other
isMirror :: Tree a -> Tree a -> Bool
isMirror Empty Empty = True
isMirror (Branch _ l1 r1) (Branch _ l2 r2) = (isMirror l1 r2) && (isMirror r1 l2)
isMirror _ = False

-- then determine whether a tree is symmetric
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = isMirror l r

-- test
test :: [Bool]
test = [
  (symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)) == False,
  (symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))) == True]

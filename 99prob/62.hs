-- collect the leaves of a binary tree
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show)

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

-- leaves (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) == [4, 2]
-- leaves (Branch 1 Empty Empty) = [1]

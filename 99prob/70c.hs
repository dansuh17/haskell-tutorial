-- count the nodes in a multiway tree
data Tree a = Node a [Tree a] deriving (Show, Eq)

nnodes :: Tree a -> Int
nnodes (Node _ forest) = 1 + sum (map nnodes forest)

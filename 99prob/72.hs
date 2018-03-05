-- construct the bottom-up order sequence of the tree nodes
data Tree a = Node a [Tree a] deriving (Show, Eq)

bottom_up :: Tree Char -> String
bottom_up (Node x forest) = (concat $ map bottom_up forest) ++ [x]


-- same function using concatMap
bottom_up2 :: Tree Char -> String
bottom_up2 (Node x ts) = concatMap bottom_up ts ++ [x]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

-- more efficient version using accumulator
bottomUp :: Tree a -> [a]
bottomUp t = bottomUpAux t []
  where bottomUpAux :: Tree a -> [a] -> [a]
        bottomUpAux (Node x ts) xs = foldr bottomUpAux (x:xs) ts

-- dotstring representation of binary trees
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = x:(tree2ds l ++ tree2ds r)

ds2tree :: String -> Tree Char
ds2tree = fst . ds2treeHelp

ds2treeHelp :: String -> (Tree Char, String)
ds2treeHelp ('.':xs) = (Empty, xs)
ds2treeHelp (x:xs) = ((Branch x leftTree rightTree), rightRem)
  where (leftTree, leftRem) = ds2treeHelp xs
        (rightTree, rightRem) = ds2treeHelp leftRem


tree67 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                                (Branch 'e' Empty Empty))
                    (Branch 'c' Empty
                                (Branch 'f' (Branch 'g' Empty Empty) Empty))

-- constructing binary search trees
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

construct :: [Int] -> Tree Int
construct xs = foldl (flip addElemTree) Empty xs  -- note the use of 'flip'

addElemTree :: Int -> Tree Int -> Tree Int
addElemTree x Empty = Branch x Empty Empty
addElemTree x (Branch y t1 t2)
  | x < y = Branch y (addElemTree x t1) t2
  | otherwise = Branch y t1 (addElemTree x t2)

-- more elegant solution of addElemTree
addElemTree2 :: Ord a => a -> Tree a -> Tree a
addElemTree2 x Empty = Branch x Empty Empty
addElemTree2 x t@(Branch y l r) = case compare x y of  -- @ notation can be used in this case also
                                   LT -> Branch y (addElemTree2 x l) r
                                   GT -> Branch y l (addElemTree2 x r)
                                   EQ -> t  -- handles equality case

test57 :: [Bool]
test57 = [
  (construct [3, 2, 5, 7, 1]) == (Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty)))]

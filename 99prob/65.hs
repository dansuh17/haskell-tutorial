-- alternative layout method
-- rules:
-- 1. distance from left node and right node from parent node is equal
-- 2. equal height nodes have same y-coordinate
-- from these rules, we can conclude : the most deep path determines the width
-- horizontal distance = 2 ^ h
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)


layoutAlt :: Tree Char -> Tree (Char, (Int, Int))
layoutAlt t = fmap (\(x, (w, h)) -> (x, (w + adjust, h))) offsetAnnotated
  where offsetAnnotated = annotateOffset (annotateHeight t 1) 0 (height t)
        adjust = abs $ maxNegativeOffset offsetAnnotated

maxNegativeOffset :: Tree (Char, (Int, Int)) -> Int
maxNegativeOffset Empty = 0
maxNegativeOffset (Branch (x, (w, h)) l r) = minimum [w, maxNegativeOffset l, maxNegativeOffset r]

annotateOffset :: Tree (Char, Int) -> Int -> Int -> Tree (Char, (Int, Int))
annotateOffset Empty _ _ = Empty
annotateOffset (Branch (x, h) l r) offset maxHeight = Branch (x, (offset, h)) (annotateOffset l (offset - spread) maxHeight) (annotateOffset r (offset + spread) maxHeight)
  where spread = 2 ^ (maxHeight - h - 1)

height :: Tree Char -> Int
height Empty = 0
height t@(Branch _ l r) = 1 + maximum [height l, height r]

annotateHeight :: Tree Char -> Int -> Tree (Char, Int)
annotateHeight Empty _ = Empty
annotateHeight (Branch x l r) h = Branch (x, h) (annotateHeight l (h + 1)) (annotateHeight r (h + 1))


tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )


-- other solutions
layout :: Tree a -> Tree (a, Pos)
layout t = layoutAux x1 1 sepl t
  where d = depth t
        ld = leftdepth t
        x1 = 2 ^ (d - 1) - 2 ^ (d - ld) + 1
        sepl = 2 ^ (d - 2)
        layoutAux x y sep Empty = Empty
        layoutAux x y sep (Branch a l r) =
            Branch (a, (x, y)) (layoutAux (x - sep) (y + 1) (sep `div` 2) l)
                               (layoutAux (x + sep) (y + 1) (sep `div` 2) r)

depth :: Tree a -> Int
depth Empty = 0
depth (Branch a l r) = max (depth l) (depth r) + 1

leftdepth :: Tree a -> Int
leftdepth Empty = 0
leftdepth (Branch a l r) = leftdepth l + 1

import Data.List (sortBy, groupBy)
import Data.Function (on)

-- sort a list of lists according to length of sublists
-- similar implementation of quicksort
lsort :: Eq a => [[a]] -> [[a]]
lsort [] = []  -- base case
lsort xs = (lsort [z | z <- xs, (length z) < (length x)])
           ++ [v | v <- xs, (length v) == (length x)]  -- pivot
           ++ (lsort [y | y <- xs, (length y) > (length x)])
  where
    mid = (length xs) `quot` 2  -- pick middle element
    x = xs !! mid

-- sort by length frequency - order in ascending order of length frequency
lfsort :: Eq a => [[a]] -> [[a]]
lfsort = concat . sortByLength . grouped
  where
    -- here, groupBy requires Bool but sortBy requires Ordering
    grouped = groupBy ((==) `on` length) . sortByLength
    sortByLength = sortBy (compare `on` length)

-- simple function using sortOn :: Ord b => (a -> b) -> [a] -> [a]
-- faster than sortBy since it evaluates the (a -> b) function only once
lsort2 = sortOn length

-- using compaing :: Ord a => (b -> a) -> b -> b -> Ordering
import Data.Ord (comparing)
lsort3 :: [[a]] -> [[a]]
lsort3 = sortBy (comparing length)

-- same approach as my lfsort but in different wording
lfsort2 :: [[a]] -> [[a]]
lfsort2 lists = concat groups
  where groups = lsort $ groupBy equalLength $ lsort lists
        equalLength xs ys = length xs == length ys

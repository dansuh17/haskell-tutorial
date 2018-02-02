-- slice an array at position n ~ m
-- my solution:
myslice :: [a] -> Int -> Int -> [a]
myslice xs n m = take (m - n + 1) $ drop (n - 1) xs

slice2 xs i k | i > 0 = take (k - i + 1) $ drop (i - 1) xs

-- using Maybe
slice3 :: [a] -> Int -> Int -> Maybe [a]
slice3 [] _ _ = Just []
slice3 xs k n
  | k == n = Just []
  | k > n || k > length xs || n > length xs || k < 0 || n < 0 = Nothing
  | k == 0 = Just (take n xs)
  | otherwise = Just (drop (k - 1) $ take n xs)

-- using list comprehension (make a tuple along with the index)
slice4 xs i k = [x | (x, j) <- zip xs [1..k], i <= j]

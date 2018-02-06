-- remove nth element from a list
myRemoveAt :: Int -> [a] -> (a, [a])
myRemoveAt n xs = (xs !! (n - 1), (take (n - 1) xs) ++ (drop n xs))

-- safe version
removeAt2 n xs | n > 0 && n <= length xs = (Just (xs !! index), take index xs ++ drop n xs)
               | otherwise = (Nothing, xs)
               where index = n - 1
 

-- using list comprehension
removeAt3 n xs = ((xs !! (n - 1)), [x | (i, xs) <- zip [1..] xs, i /= n])

-- drop every nth element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (dropLastIfFull n $ take n xs) ++ dropEvery (drop n xs) n
  where
    dropLastIfFull n xs = if length xs == n then init xs else xs

-- clever approach using zip and cycle
-- i think version 2 is much cleaner and cleverer
dropEvery2 xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])
dropEvery3 xs n = map fst $ filter (\(x, i) -> i `mod` n /= 0) $ zip xs [1..]

-- zip and list comprehension + mod
dropEvery4 xs n = [i | (i, c) <- (zip xs [1, 2..]), (mod c n) /= 0]

-- using guards
dropEvery5 xs n
  | length xs < n = xs
  | otherwise = take (n - 1) xs ++ dropEvery5 (drop n xs) n 

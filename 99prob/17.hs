split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

split2 = flip splitAt

-- &&& ??????
split3 (x:xs) n | n > 0 = (:) x . fst &&& snd $ split xs (n - 1)
split3 xs _ = ([], xs)  -- case n == 0

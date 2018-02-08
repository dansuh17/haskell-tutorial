-- insert alement at a given position into list
myinsert :: a -> [a] -> Int -> [a]
myinsert x xs n = (take (n - 1) xs) ++ [x] ++ (drop (n - 1) xs)

insert2 x xs (n + 1) = let (ys, zs) = split xs n in ys ++ x:zs

insert3 el lst n = fst $ foldl helper ([], 1) lst
  where helper (acc, i) x = if i == n then (acc ++ [el, x], i + 1) else (acc ++ [x], i + 1)

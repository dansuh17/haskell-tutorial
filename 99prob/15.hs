-- replicate the elements of a list given number of times
repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> take n $ repeat x) xs

repli2 = flip $ concatMap . replicate

-- using list monad
repli3 xs n = xs >>= replicate n

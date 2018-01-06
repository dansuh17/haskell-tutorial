-- pack consecutive duplicates of list elements into sublists
myPack' :: (Eq a) => [a] -> [[a]]
myPack' xs = foldr f [[last xs]] xs
  where
    f x as@(a:at) = if x == head a then (x:a):at else [x]:as

-- using 'span' :: (a -> Bool) -> [a] -> ([a], [a])
pack (x:xs) = let (first, rest) = span (== x) xs
              in (x:first) : pack rest

-- wrong solution using 'filter'
pack' [] = []
pack' (x:xs) = (x:(filter (==x) xs)):(pack' $ filter (/=x) xs)

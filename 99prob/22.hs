-- create list containing integers within a given range
myrange :: Int -> Int -> [Int]
myrange s e = [s..e]

range2 :: Int -> Int -> [Int]
range2 = enumFromTo

range3 n m
  | n == m = [n]
  | n < m = n:(range (n + 1) m)
  | n > m = n:(range (n - 1) m)  -- reverse order

range4 a b | (a == b) = [a]
range4 a b = a:range((if a < b then succ else pred) a) b) -- succ :: a -> a, adds 1

-- scanl is similar to foldl, but returns a list of successive reduced values from the left
-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
-- scanl function init_acc list -> result list
range5 l r = scanl (+) l (replicate (l - r) 1)

-- support of reverse order
range6 l r = scanl op l $ replicate diff 1
  where
  op = if 1 < r then (+) else (-)
  diff = abs $ l - r

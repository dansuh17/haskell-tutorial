-- determine the prime factors of a positive integer
primeFactors :: Int -> [Int]
primeFactors n
  | n == 1 = []
  | otherwise = let sd = (smallestDiv n)
                in [sd] ++ primeFactors (n `quot` sd)
  -- smallestDiv finds the smallest divisor larger than 1
  where smallestDiv n = head $ dropWhile (\a -> (n `mod` a) /= 0) [2..n]

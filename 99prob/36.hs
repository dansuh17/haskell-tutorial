import Data.List (group)
-- determine the prime factors of a given positive integer
-- do so by constructing a list of prime factors and their multiplicity
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map (\x -> (head x, length x)) . group $ primeFactors n  -- simply group them and count
  where primeFactors m | m == 1 = []
                       | otherwise = (smallestPrime m) : primeFactors (m `quot` (smallestPrime m))
        smallestPrime m = head $ dropWhile (\x -> (m `mod` x) /= 0) [2..m]

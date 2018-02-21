-- calculate Euler's totient function phi(m)
-- first section is just a copy of problem 36
import Data.List (group)
-- determine the prime factors of a given positive integer
-- do so by constructing a list of prime factors and their multiplicity
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map (\x -> (head x, length x)) . group $ primeFactors n  -- simply group them and count
  where primeFactors m | m == 1 = []
                       | otherwise = (smallestPrime m) : primeFactors (m `quot` (smallestPrime m))
        smallestPrime m = head $ dropWhile (\x -> (m `mod` x) /= 0) [2..m]

-- main part
phi :: Int -> Int
phi n = foldl (\a (p, m) -> a * (p - 1) * p ^ (m - 1)) 1 $ primeFactorsMult n

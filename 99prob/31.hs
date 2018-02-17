-- determine whether a given integer number is prime
-- helper function for determining the prime
filterDivisible :: Int -> Int -> Bool
filterDivisible n d
  | d == 1 = True  -- n is prime if it reaches to testing divisibility by 1
  | d == n = filterDivisible n $ ceiling . sqrt $ fromIntegral d
  | otherwise = if (n `mod` d) == 0 then False else filterDivisible n (d - 1)

-- main isprime function
myIsPrime :: Int -> Bool
myIsPrime n
  | n < 2 = error "Cannot define prime"
  | otherwise = filterDivisible n n



-- efficient solution using Eratosthenes sieve
isPrime2 :: Integral a => a -> Bool
isPrime2 k = k > 1 && foldr (\p r -> p * p > k || k `rem` p /= 0 && r) True primesTME

{-# OPTIONS_GHC -O2 -fno-cse #-}
-- tree-merging Eratosthenes' sieve : see https://wiki.haskell.org/Prime_numbers
primesTME = 2 : gaps 3 (join [[p *p, p * p + 2 *p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p * p, p * p + 2 * p ..] | p <- primes'])
    join ((x:xs):t) = x : union xs (join (pairs t))
    pairs ((x:xs):ys:y) = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k == x = gaps (k + 2) t
                    | True = k : gaps (k + 2) xs

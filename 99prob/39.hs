-- list of prime numbers between lower and upper bound
primesR :: Integral a => a -> a -> [a]
primesR a b | even a = filter isPrime [a + 1, a + 3 .. b]
            | True = filter isPrime [a, a + 2 .. b]
  where
        isPrime :: Integral a => a -> Bool
        isPrime n | n < 2 = False
                  | n == 2 = True
                  | otherwise = all (\x -> (n `mod` x /= 0) && True) [2..(ceiling . sqrt . fromIntegral $ n)]

-- bounded prime numbers using Sieve of Eratosthenes
primesR2 :: Integral a => a -> a -> [a]
primesR2 a b = takeWhile (<= b) $ dropWhile (< a) $ sieve [2..]
  where sieve (n:ns) = n:sieve [m | m <- ns, m `mod` n /= 0]

-- calculate Euler's totient function phi(m)
-- which is defined as the number of positive integers r(1 <= r < m) that are coprime to m
totient :: Int -> Int
totient 1 = 1
totient m = length . filter (coprime m) [1..(m - 1)]
  where
    coprime a b = gcd a b == 1


totient2 :: Int -> Int
totient2 n = length [x | x <- [1..n], coprime x n]

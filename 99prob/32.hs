-- gcd using Euclid's algorithm
myGcd :: Int -> Int -> Int
myGcd a b
  | b == 0 = abs a
  | otherwise = myGcd b (a `mod` b)

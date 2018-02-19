-- determining coprimes
-- here I used my own definition of gcd, but gcd function in Prelude can be used instead
myCoprime :: Int -> Int -> Bool
myCoprime a b = myGcd a b == 1
  where
    myGcd a b | a < 0 = myGcd (abs a) b
              | b == 0 = a
              | otherwise = myGcd (a - b) a

-- ranged goldbach composition
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList l h = map goldbach $ filter even [l..h]

-- give a minimum prime number
goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' low high thres = filter (\(x, y) -> (x >= thres) && (y >= thres)) $ goldbachList low high

-- using problem 40...
-- goldbach's conjecture
goldbach :: Int -> (Int, Int)
goldbach n
  | odd n || n <= 2 = error "Input must be an even number greater than 2"
  | otherwise = head 
        . map (\k -> (k, n - k))
        . filter (\x -> isPrime x && isPrime (n - x))
        . takeWhile (\a -> a <= (n `quot` 2)) $ primes
    where isPrime p = all (\m -> p `mod` m /= 0) [2..(floor . sqrt . fromIntegral $ p)]
          primes = sieve [2..]
          sieve (n:ns) = n:sieve [m | m <- ns, m `mod` n /= 0]

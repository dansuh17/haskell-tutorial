-- all functions use only one parameter
-- for example: :t max == max :: (Ord a) => a-> a -> a

-- several arguments
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
multTwoWithNine = multThree 9

-- partial application : compare has type a -> (a -> Ordering)
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100


-- partial application for infix functions
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


-- another example
isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])


-- implementation of zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys


-- flip implementation
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x


-- map implementation
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs


-- filter implementation
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs
filter (>3) [1, 5, 3, 2, 1, 6, 4, 3, 2, 1]


-- quicksort another
quicksort :: (Ord a) => [a] -> [a]
quicksort (x:xs) = 
    let smaller = quicksort (filter (<=x) xs)
        larger = quicksort (filter (>x) xs)
    in smaller ++ [x] ++ larger

-- largest number that is divisible by 3829
largeDiv :: (Integral a) => a
largeDiv = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

-- Collatz sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- use map to create infinite list of functions
let listOfFuns = map (*) [0..]
(listOfFuns !! 4) 5 -- becomes 20

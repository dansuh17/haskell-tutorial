-- lambda notation : (\
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

map (\(a, b) -> a + b) [(1, 2), (3, 5), (6, 3), (2, 6), (2, 5)]

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

-- use of lambda and foldl
-- foldl + function taking (accumulated val, next elem) + starting val + list
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- implementation of functions using fold
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

sum' :: (Num a) => [a] -> a
sum' = foldl1 (+)

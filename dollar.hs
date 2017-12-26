-- function application : right associative
($) :: (a -> b) -> a -> b
f $ x = f x

-- the two expressions below are equal
sum (filter (>10) (map (*2) [2..10]))
sum $ filter (>10) $ map (*2) [2..10]


-- function composition : dot
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- the two expressions below are equivalent
map (\x -> negate (abs x)) [5, -3, -6, 7, -3, 2, -19, 24]
map (negate . abs) [5, -3, -6, 7, -3, 2, -19, 24]


-- function that finds sum of all odd squares that are smaller than 10,000
-- below are three different implementation
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
oddSquareSum =
    let oddSquare = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquare
    in sum belowLimit

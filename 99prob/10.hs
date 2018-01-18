-- run-length encoding of a list
-- use the function defined in #9
myPack :: (Eq a) => [a] -> [[a]]
myPack xs = foldr f [[last xs]] xs
  where
    f x as@(a:at) = if x == head a then (x:a):at else [x]:as

-- encode "mississippi" == [(1, "m"), (1, "i"), (2, "s"), (1, "i"), (2, "s"), (1, "i"), (2, "p"), (1, "i")]
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (myPack xs)

encode2 xs = [(length x, head x) | x <- group xs]

encode3 :: Eq a => [a] -> [(Int, a)]
encode3 = map (\x -> (length x, head x)) . group

-- abusing Control.Arrow
encode4 :: Eq a => [a] -> [(Int, a)]
encode4 xs = map (length &&& head) $ group xs

-- using <$> and <*>
-- <$> is like infix version of fmap : Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b
encode5 :: map ((,) <$> length <*> head) . pack

encode6 [] = []
encode6 (x:xs) = (length $ x : takeWhile (==x) xs, x) : encode (dropWhile (==x) xs)

encode7 [] = []
encode7 (x:xs) = encode'' 1 x xs where
    encode'' n x [] = [(n, x)]
    encode'' n x (y:ys)
        | x == y = encode'' (n + 1) x ys
        | otherwise = (n, x) : encode'' 1 y ys

import List
encode8 xs = zip (map length l) h where
    l = (group xs)
    h = map head l

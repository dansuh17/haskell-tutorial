-- eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs = reverse $ foldl (\a x -> case a of [] -> [x] 
                                                 (y:ys) -> if y == x then a else x : a) [] xs

-- other solutions
import Data.List
compress' = map head . group   -- group :: (Eq a) => [a] -> [[a]]

compress'' = (x:ys@(y:_))
    | x == y = compress ys
    | otherwise = x : compress ys
compress'' ys = ys

-- 도저히 이해가 안되는 솔루션
compress''' xs = foldr f (const []) xs Nothing
  where
    f x r a@(Just q) | x == q = r a
    f x r _ = x : r (Just x)

-- 깔끔한 방식
compress'''' [] = []
compress'''' (x:xs) = x : (compress $ dropWhile (== x) xs)


-- using foldr
compress1' xs = foldr (\a b -> if a == (head b) then b else a : b) [last x] x

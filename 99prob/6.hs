-- determining palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && (isPalindrome . init $ xs)
-- or
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

-- using foldl just for fun
isPalindrome' xs = foldl (\acc (a, b) -> if a == b then acc else False) True input
    where input = zip xs (reverse xs)

-- another one just for fun using liftM2
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- ex) liftM2 (+) [0, 1] [0, 2] = [0, 1, 2, 3]
isPalindrome'' = Control.Monad.liftM2 (==) id reverse

-- fun 3
isPalindrome''' = (==) Control.Applicative.<*> reverse

-- zipWith
isPalindrome'''' xs = foldr (&&) True $ zipWith (==) xs (reverse xs)

-- zipWith2
isPalindrome''''' xs = and $ zipWith (==) xs (reverse xs)

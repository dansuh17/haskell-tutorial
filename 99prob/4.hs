-- find number of elements of list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength1 = foldl (\n _ -> n + 1) 0
myLength2 = foldr (\_ n -> n + 1) 0
myLength3 = foldr (\_ -> (+1)) 0
myLength4 = foldr ((+) . (const 1)) 0

myLength' = fst . last . zip [1..]

myLength'' = sum . map (\_ -> 1)

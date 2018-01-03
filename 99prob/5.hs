reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]  -- wasteful because it conses everything as it is reconstructed

reverse'' list = reverse''' list [] 
    where 
    reverse''' [] reversed = reversed
    reverse''' (x:xs) reversed = reverse''' xs (x:reversed)


reverse'''' xs = foldr (\x fId empty -> fId (x:empty)) id xs []

myReverse = foldl (\a x -> x:a) []



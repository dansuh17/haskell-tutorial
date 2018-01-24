-- duplicate elements in list
dupli :: [a] -> [a]
dupli = concatMap dup
  where
    dup x = [x, x]


dupli2 [] = []
dupli2 (x:xs) = x:x:dupli xs

dupli3 list = concat [[x, x] | x <- list]

dupli4 = foldl (\a x -> a ++ [x, x]) []

dupli5 = foldr (\x a -> x:x:xs) []

-- Finding the kth element of a list
elementAt :: [a] -> Int -> a
elementAt list i = list !! (i + 1)

elementAt' (x:_) 1 = x
elementAt' [] _ = error "Index out of bounds"
elementAt' (_:xs) k
    | k < 1 = error "Index out of bounds"
    | otherwise = elementAt' xs (k - 1)

elementAt'' :: [a] -> Int -> a
elementAt'' xs n
    | length xs < n = error "Index out of bounds"
    | otherwise = fst . last $ zip xs [1..n]


elementAt''' xs n
    | length xs < n = error "Index out of bounds"
    | otherwise = last $ take n xs

elementAt'''' xs n
    | length xs < n = error "Index out of bounds"
    | otherwise = head . reverse $ take n xs

elementAt''''' = (last .) . take . (+ 1)

myLast :: (Show a, Num a) => [a] -> a
myLast [] = error "No end for empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' = foldr1 (const id)

myLast'' = head . reverse

myLast''' xs = xs !! (length xs - 1)

myLast'''' = last

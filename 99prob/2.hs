myButLast :: [a] -> a
myButLast = last . init

myButLast' x = reverse x !! 1

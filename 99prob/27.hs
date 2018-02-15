myCombination :: Int -> [a] -> [([a], [a])]
myCombination 0 xs = [([], xs)]
myCombination n [] = []
myCombination n (x:xs) = ts ++ ds
  where
    ts = [(x:ys, zs) | (ys, zs) <- myCombination (n - 1) xs]
    ds = [(ys, x:zs) | (ys, zs) <- myCombination n xs]

myGroup :: [Int] -> [[a]] -> [[[a]]]
myGroup [] _ = [[]]
myGroup (n:ns) xs = [g:gs | (g, rs) <- myCombination n xs, gs <- group ns rs]

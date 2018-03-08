module EightyThree where

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

k4 = Graph ['a', 'b', 'c', 'd'] [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]


paths' :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths' a b xs | a == b = [[a]]
              | otherwise = concat [map (a:) $ paths' d b $ [x | x <- xs, x /= (c, d)]
                                    | (c, d) <- xs, c == a] ++
                            -- this assumes undirected graph
                            concat [map (a:) $ paths' c b $ [x | x <- xs, x /= (c, d)]
                                    | (c, d) <- xs, d == a]



cycle' :: (Eq a) => a -> [(a, a)] -> [[a]]
cycle' a xs = [a : path | e <- xs, fst e == a,
                          path <- paths' (snd e) a [x | x <- xs, x /= e]] ++
              [a : path | e <- xs, fst e == a,
                          path <- paths' (fst e) a [x | x <- xs, x /= e]]

spantree :: (Eq a) => Graph a -> [Graph a]
spantree (Graph ns es) = filter (allPaths) $ filter (not . cycles) $ filter (nodes) alltrees
  where alltrees = [Graph (nodes edges) edges | edges <- (foldr (\x acc -> map (x:) acc ++ acc) [[]] edges)]
        hasAllNodes (Graph nns _) = length ns == length nns
        nodes edgs = foldl (\acc (x, y) -> addIfNotExist x $ addIfNotExist y acc) [] edgs
        addIfNotExist n as = if n `elem` as then as else n:as
        cycles = (Graph xs' ys') = any ((/=) 0 . length . flip cycle' ys') xs'
        allPaths (Graph (x':xs) ys') = not $ any (null) [paths' x' y' ys' | y' <- ys']

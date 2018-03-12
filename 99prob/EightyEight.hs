module EightyEight where

data Graph a = Graph [a] [(a, a)]

-- connected components
connectedComponents :: Eq a => Graph a -> [[a]]
connectedComponents (Graph [] _) = []
connectedComponents (Graph (n:ns) es) = subGraphNodes:(connectedComponents (Graph (notConn ns subGraphNodes) es))
  where
    subGraphNodes = connectedAll [n] []  -- find reachable nodes from n
    -- do a depth first traversal of a given node
    connectedAll [] _ = []
    connectedAll toVisit@(n:ns) visited = n:(connectedAll newToVisit (n:visited))
      where newToVisit = [y | (x, y) <- es, x == n, not $ y `elem` visited]
              ++ [x | (x, y) <- es, y == n, not $ x `elem` visited]
    notConn ns connected = [x | x <- ns, not $ x `elem` connected]  -- unreachable nodes

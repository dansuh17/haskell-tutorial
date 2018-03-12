module EightySeven where

-- depth fisrt traversal of a graph
depthFirst :: Eq a => ([a], [(a, a)]) -> a -> [a]
depthFirst (ns, es) startNode = depthFirstAux (ns, es) [] [startNode]

-- helper function that uses stack
depthFirstAux :: Eq a => ([a], [(a, a)]) -> [a] -> [a] -> [a]
depthFirstAux _ _ [] = []  -- stack empty
depthFirstAux (ns, es) visited toVisit@(startNode:restToVisit) = (startNode:)
  $ depthFirstAux (ns, es) (startNode:visited) newToVisit  -- recursive call
    where connected = [y | (x, y) <- es, x == startNode] ++ [x | (x, y) <- es, y == startNode]
          isVisited x = x `elem` visited
          isInQueue x = x `elem` restToVisit
          -- new stack contents
          newToVisit = (filter (\n -> not (isVisited n) && not (isInQueue n)) connected) ++ restToVisit

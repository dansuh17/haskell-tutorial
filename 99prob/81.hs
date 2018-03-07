-- path from one node to another node
data Adjacency a = Adj [(a, a)] deriving (Show, Eq)
paths :: Int -> Int -> Adjacency a
paths from to (Adj g)
  | from == to = [[to]]
  | otherwise = [
      from:path | edge <- g, (fst edge) == from,
      path <- (paths (snd edge) to [e | e <- edges, e /= edge])
    ];

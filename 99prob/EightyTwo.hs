module EightyTwo where

import Prelude hiding(cycle)
import Data.List (partition)

-- calculate the paths from start -> to
paths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths start = pathsAux [start] start

-- need more information to actually calculate
pathsAux :: (Eq a) => [a] -> a -> a -> [(a, a)] -> [[a]]
pathsAux previous start end graph
  | start == end = [previous]
  | otherwise = concat [pathsAux (previous ++ [newStart]) newStart end graph | newStart <- connected]
    where
      connected = foldl (\acc (from, to) -> if from == start && (not $ to `elem` previous) then to:acc else acc) [] graph

-- find cycles in the graph
cycle :: (Eq a) => a -> [(a, a)] -> [[a]]
cycle point graph = map (\xs -> xs ++ [point])  -- append the final arrival point (== starting point)
    $ concat [pathsAux [point] point oneBefore graph | oneBefore <- befores]
  where
    -- points that can go to starting point (== arriving point)
    befores = foldl (\acc (from, to) -> if to == point then from:acc else acc) [] graph

-- examples)
-- cycle 2 [(1, 2), (2, 3), (1, 3), (3, 4), (4, 2), (5, 6)] == [[2,3,4,2]]
-- cycle 1 [(1, 2), (2, 3), (1, 3), (3, 4), (4, 2), (5, 6)] == []

-- alternative solution, but it seems to be breaking
{-cycle' :: Int -> [(Int, Int)] -> [[Int]]
cycle' n g = search n []
  where search [] result = result
        search cur result = search (go active) (arrive ++ result)  -- this part is wierd
          where
            split = partition end cur  -- end partition
            end s = (last s == n) && (length s /= 1)  -- isEnd
            active = snd split
            arrive = fst split-}

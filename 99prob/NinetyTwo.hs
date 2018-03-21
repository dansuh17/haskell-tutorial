module NinetyTwo where

import Data.List (permutations, sort, nub)
import Data.Array (listArray, (!))
import Control.Monad (guard)

permutation :: Eq a => [a] -> [[a]]
permutation [] = []
permutation [x] = [[x]]
permutation xs = [ x:perm | x <- xs, perm <- permutation [y | y <- xs, y /= x]]

-- find the solution for Von Koch's conjecture
vonKoch :: [(Int, Int)] -> [[Int]]
vonKoch edges = filter isEqualLength  -- check equality of duplicate-removed list of differences of edge numbers
  $ map (\perm ->
          nub $ map (\ edge -> calcDifference perm edge) edges)  -- remove duplicates of differences
        $ perms
  where
    calcDifference :: [Int] -> (Int, Int) -> Int
    calcDifference nodeSeq (x, y) = abs (nodeSeq !! (x - 1) - nodeSeq !! (y - 1))
    isEqualLength xs = length xs == len
    len = length edges + 1
    nodes = [1..len]
    perms = permutation nodes  -- all possible number assignments for nodes


-- predicate for Von Koch's conjecture
vonKoch'' edges = do
    let n = length edges + 1
    nodes <- permutations [1..n]
    let nodeArray = listArray (1, n) nodes
    let dists = sort $ map (\ (x, y) -> abs (nodeArray ! x - nodeArray ! y)) edges
    guard $ and $ zipWith (/=) dists (tail dists)
    return nodes

-- [6, 7, 8, 9, 3, 4, 10, 11, 5, 12, 2, 13, 14, 1] `elem` vonKoch [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)]

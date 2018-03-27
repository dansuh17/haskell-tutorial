module NinetyFour where

import Data.List (permutations, nub)
import Data.Maybe (catMaybes, isJust)
import EightyFive (iso)

data Graph a = Graph [a] [(a, a)] deriving (Show)
-- using iso from module EightyFive for testing equality of two graphs:
-- apparently this is not working very well :(
instance Eq a => Eq (Graph a) where
  (==) (Graph n1 e1) (Graph n2 e2) = iso (n1, e1) (n2, e2)

-- generate each deg number of nodes and generate edges (all combinations)
-- and then filter them with isomorphic test
regular :: Int -> Int -> [Graph Int]
regular numNode deg =
  if odd numNode && odd deg
  then []
  else nub [Graph [1..numNode] e | e <- edges]
  where
    edges = chooseEdges $ concatMap (replicate deg) [1..numNode]

chooseEdges :: [Int] -> [[(Int, Int)]]
chooseEdges ns = catMaybes . map splitByNum $ permutations ns


splitByNum :: Ord a => [a] -> Maybe [(a, a)]
splitByNum xs
  | isPossible edgeChosen = Just edgeChosen
  | otherwise = Nothing
  where
    (es, os) = splitOddEven xs
    orderEdges = map (\ (x1, x2) -> if x1 > x2 then (x1, x2) else (x2, x1))
    edgeChosen = orderEdges $ zip es os
    isPossible es = (all id . map (\(x1, x2) -> x1 /= x2) $ es) && (length es) == (length . nub $ es)

splitOddEven :: [a] -> ([a], [a])
splitOddEven xs = mapToTuple (\xs -> map fst xs) (filter (odd . snd) xsWithIdx, filter (even . snd) xsWithIdx)
  where
    xsWithIdx = zip xs [0..]

mapToTuple :: (a -> b) -> (a, a) -> (b, b)
mapToTuple f (x1, x2) = (f x1, f x2)

module EightyFive where

-- graph isomorphism
graphG1 = ([1,2,3,4,5,6,7,8], [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)])
graphH1 = ([1,2,3,4,5,6,7,8], [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)])

-- check equality with all possible permutations of second graph
iso :: Eq a => ([a], [(a, a)]) -> ([a], [(a, a)]) -> Bool
-- check for all possible node mappings
iso g1@(n1, e1) g2@(n2, e2) = any (isGraphEqual g1) transformedGraphs
  where
    transformedGraphs = [transformGraph g2 mapping | mapping <- permutationPairs g1 g2]

-- get all permutations of the list
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [n:oneLessPerm | n <- xs, oneLessPerm <- (permutations $ exceptX xs n)]

-- get a list except an element
exceptX :: (Eq a) => [a] -> a -> [a]
exceptX xs x = [n | n <- xs, n /= x]

-- two permutation pairs are equal
permutationPairs :: Eq a => ([a], [(a, a)]) -> ([a], [(a, a)]) -> [[(a, a)]]
permutationPairs (n1, _) (n2, _) = map (\n2p -> zip n1 n2p) $ permutations n2

-- transform a graph according to the mapping
transformGraph :: (Eq a) => ([a], [(a, a)]) -> [(a, a)] -> ([a], [(a, a)])
transformGraph g2@(n2, e2) mapping = (newNodes, newEdges)
  where
    newNodes = foldl (\acc node -> (findNode mapping node):acc) [] n2
    findNode mapping node = fst $ head $ dropWhile (\p -> snd p /= node) mapping
    newEdges = foldl (\acc (from, to) -> (findNode mapping from, findNode mapping to):acc) [] e2

-- check if two lists are equal, in sense that two sets are equal
isListEqual :: (Eq a) => [a] -> [a] -> Bool
isListEqual xs ys = foldl (\acc x -> x `elem` ys && acc) True xs

-- check if two graphs are equal
isGraphEqual :: Eq a => ([a], [(a, a)]) -> ([a], [(a, a)]) -> Bool
isGraphEqual (n1, e1) (n2, e2) = isListEqual n1 n2 && isListEqual e1 e2

-- iso graphG1 graphH1 == True
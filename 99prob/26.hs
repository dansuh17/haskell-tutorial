-- generate combinations of K distinct objects chosen from N elements of a list
import Data.List

myComb :: Int -> [a] -> [[a]]
myComb 0 _ = [[]]
myComb n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n - 1) xs']

-- same implementation using 'do' notation
myComb2 :: Int -> [a] -> [[a]]
myComb2 0 _ = return []
myComb2 n xs = do y:xs' <- tails xs
                  ys <- combinations (n - 1) xs'
                  return (y:ys)

-- using subsequences in Data.List and filtering with length
-- **very heavy for huge arrays
myComb3 k ns = filter ((k ==) . length) (subsequences ns)

-- using recursive definitions without using tails or subsequences
myComb4 :: Int -> [a] -> [[a]]
myComb4 0 _ = return []
myComb4 n xs = [xs !! i : prevComb | i <- [0..(length xs) - 1],  -- all possible indices
    prevComb <- myComb4 (n - 1) (drop (i + 1) xs)]  -- combinations with 1 less elements
    -- TODO: what if drop (i + 1) xs has less than n - 1 elements?

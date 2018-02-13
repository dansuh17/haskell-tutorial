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
myComb3 k ns = filter ((k ==) . length) (subsequences ns)

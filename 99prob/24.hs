import System.Random
import Data.List
import Control.Monad (replicateM)
import Control.Applicative

myRndSelect :: Int -> Int -> IO [Int]
myRndSelect n m = replicateM n $ getStdRandom $ randomR (1, m)


myRndSelect2 n to = diff_select n [1..to]

diff_select 0 _ = return []
diff_select _ [] = error "too few elements to choose from"
diff_select n xs = do r <- randomRIO (0, (length xs) - 1)  -- randomRIO :: (a, a) -> IO a
                      let remaining = take r xs ++ drop (r + 1) xs  -- make distinct numbers
                      rest <- diff_select (n - 1) remaining
                      return ((xs !! r) : rest)


diff_select2 :: Int -> Int -> IO [Int]
diff_select2 n m = do
  gen <- getStdGen
  return . take n $ randomRs (1, m) gen


diff_select3 :: Int -> Int -> StdGen -> [Int]
-- randomRs :: RandomGen g => (a, a) -> g -> [a]
diff_select3 n m = take n . nub . randomRs (1, m)  -- remove duplicates from list

diff_select4 :: Int -> Int -> IO [Int]
-- getStdGen :: IO StdGen
-- <$> here does :: (StdGen -> [Int]) -> IO StdGen -> IO [Int]
diff_select4 n m = take n . nub . randomRs (1, m) <$> getStdGen

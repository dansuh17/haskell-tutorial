import System.Random
import Control.Monad (replicateM)

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
-- replicateM : perform an action n times
-- getStdRandom :: (StdGen -> (a, StdGen)) -> IO a
-- randomR :: (a, a) -> g -> (a, g)
rnd_select 1 n
  | n < 0 = error "N must be greater than zero."
  | otherwise = do pos <- replicateM n $  -- unwrap IO Int -> Int
                            getStdRandom $ randomR (0, (length 1) - 1)
                   return [l!!p | p <- pos] -- wrap into IO

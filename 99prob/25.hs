import Data.List
import System.Random (randomRs, getStdGen, randomRIO)

-- random permutation of a list
-- my own function
randPermu :: [a] -> IO [a]
randPermu xs = (\ns -> [(xs !! n) | n <- ns]) <$> permIO -- turn indices into elements in IO [Int] type
  where
    -- create random indices
    permIO = take (length xs) . nub . randomRs (0, (length xs) - 1) <$> getStdGen

-- recursive definition
rndperm2 :: [a] -> IO [a]
rndperm2 [] = return []
rndperm2 (x:xs) = do
  -- "draw from"
  rand <- randomRIO (0, (length xs))
  rest <- rndperm2 xs
  return $ let (ys, zs) = splitAt rand rest
           in ys++(x:zs)

-- another recursive definition
rndperm3 [] = return []
rndperm3 xs = do
  rand <- randomRIO (0, (length xs) - 1)
  rest <- let (ys,(_:zs)) = splitAt rand xs
          in rndperm3 $ ys ++ zs
  return $ (xs !! rand):rest

-- using permutations function
import Data.List (permutations)
rndElem :: [a] -> IO a
rndElem xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

rndPermutation :: [a] -> IO [a]
rndPermutation xs = rndElem . permutations $ xs  -- generates all available permutations

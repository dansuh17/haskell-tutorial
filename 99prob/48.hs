import Control.Monad (replicateM)

-- extend the table function to receive any number of logical variables
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn $ [concatMap (\x -> (show x) ++ " ") arg ++ (show $ f arg)| arg <- args n]
  where
    args n = replicateM n [True, False]  -- generates combinations

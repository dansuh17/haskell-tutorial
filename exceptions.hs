import System.Environment
import System.IO
import System.IO.Error

-- similar to try -- catch statement
main = toTry `catch` handler

toTry :: IO ()
toTry = do 
  (fileName:_) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

-- exception handler
handler :: IOError -> IO ()
handler e 
  | isDoesNotExistError e = putStrLn "Whoops, had some trouble!"
  | otherwise = ioError e -- ioError :: IOException -> IO a

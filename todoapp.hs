import System.Environment
import System.Directory
import System.IO
import Data.List

{-
dispatch association list - maps command line input with corresponding function
-}
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add), ("view", view), ("remove", remove) ]

-- add a todo item
-- command line goes : todo add todo.txt
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

-- view
-- todo view todo.txt
view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName -- readFile :: FilePath -> IO String
  let todoTasks = lines contents -- lines :: String -> [String]
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks -- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  putStr $ unlines numberedTasks  -- unlines :: [String] -> String - like 'join!'

-- remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString  -- convert to Int
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

-- MAIN!!
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch  -- lookup :: a -> [(a, b)] -> Maybe b
  action args -- perform action on rest of the arguments

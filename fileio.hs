-- reading entire file and print
import System.IO

main = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

-- openFile :: FilePath -> IOMode -> IO Handle
-- hGetContents :: Handle -> IO String
-- type FilePath = String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- same thing using withFile :: FilePath -> IOMode -> (IO Handle -> IO a) -> IO a
main = do
  withFile "girlfriend.txt" ReadMode (\handle -> do
      contents <- hGetContents handle
      putStr contents)

-- definition of withFile
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle -- get the raw type 'a'...
  hClose handle -- in order to close the handle
  return result -- and then re-wrap into IO a

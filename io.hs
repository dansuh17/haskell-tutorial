-- :t puStrLn = putStrLn :: String -> IO ()
main =  putStrLn "hello, world"

-- several IO actions executed
-- each of these steps is IO action
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")

-- types having "IO <something>" means that IO returns of type <something> and binds the result using <- operator
-- getLine :: IO String

-- the above program could also have been written as:
main = do
  foo <- putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
-- where foo has type ()

-- some example program using <- and let
import Data.Char

main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- example program that reads and prints reversed
main = do
  line <- getLine
  if null line
    then return ()  -- AWARE: not like 'return' in any other imperative languages!!!
    else do  -- can only have exactly one IO action after else, so glue them using 'do'
      putStrLn $ reverseWords line
      main  -- recursive execution

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- return : transforms (or, encapsulates) a pure value into IO action
-- ex) return "haha" has type 'IO String'
main = do
  return ()
  return "HAHAHA"
  line <- getLine
  return "BLAH BLAH"
  return 4  -- doesn't do anything!
  putStrLn line

-- this program is same as...
main = do
  a <- return "hello"
  b <- return "Yeah!"
  putStrLn $ a ++ " " ++ b

-- this program
main = do
  let a = "hell"
      b = "yeah"
  putStrLn $ a ++ " " ++ b


import Data.Char
import Control.Monad

-- capslocker using I/O lazy `getContents`
main = do
  contents <- getContents  -- creates an infinite repetition of read stream
  putStr (map toUpper contents)

-- print short lines only!
main = do
  contents <- getContents
  putStr (shortLinesOnly contents)

-- or, using 'interact :: (String -> String) -> IO ()'
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input  -- splits lines into list of lines
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result

-- rewriting this as one-liner:
main = interact $ unlines . filter ((< 10) . length) . lines

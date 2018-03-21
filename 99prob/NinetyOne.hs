module NinetyOne where

-- first solution : quick implementation
import Control.Arrow
import Control.Monad
import Data.List hiding (sortOn)
import Data.Maybe
import Data.Ord
import System.Environment
import qualified Data.Map as M

-- naive backtracking approach
-- backtracking from (1, 1)
knights :: Int -> [[(Int,Int)]]
knights n = loop (n * n) [[(1,1)]]
    where loop 1 = map reverse . id
          loop i = loop (i - 1) . concatMap nextMoves
          nextMoves already@(x:xs) = [next:already | next <- possible]
              where possible = filter (\x -> on_board x && (x `notElem` already)) $ jumps x

          jumps (x,y) = [(x + a, y + b) | (a,b) <- [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]]  -- possible next coords
          on_board (x,y) = (x >= 1) && (x <= n) && (y >= 1) && (y <= n)  -- checks whether it is on board

-- solutions from : https://wiki.haskell.org/The_Knights_Tour
-- first solution
sortOn :: Ord a => (b -> a) -> [b] -> [b]
sortOn f = map snd . sortBy (comparing fst) . map (f &&& id)

clip :: (Ord a, Num a) => a -> a -> Bool
clip coord size = coord >= 0 && coord < size

valid :: (Ord a, Num a) => a -> M.Map (a, a) b -> (a, a) -> Bool
valid size solution xy@(x, y) = and [clip x size, clip y size, isNothing (M.lookup xy solution)]

neighbors :: Int -> M.Map (Int, Int) b -> (Int, Int) -> Int
neighbors size solution xy = length . filter (valid size solution) $ sequence moves xy

moves :: [(Int, Int) -> (Int, Int)]
moves = do
    f <- [(+), subtract]
    g <- [(+), subtract]
    (x, y) <- [(1, 2), (2, 1)]
    [f x *** g y]

solve :: Int -> M.Map (Int, Int) Int -> Int -> (Int, Int) -> [M.Map (Int, Int) Int]
solve size solution n xy = do
    guard (valid size solution xy)
    let solution'   = M.insert xy n solution
        sortedMoves = sortOn (neighbors size solution) (sequence moves xy)
    if n == size * size
        then [solution']
        else sortedMoves >>= solve size solution' (n+1)

printBoard :: Show a => Int -> M.Map (Int, Int) a -> String
printBoard size solution = board [0..size-1] where
    sqSize = size * size
    elemSize = length (show sqSize)
    separator = intercalate (replicate elemSize '-') (replicate (size + 1) "+")
    pad n s = replicate (elemSize - length s) ' ' ++ s
    elem xy = pad elemSize . show $ solution M.! xy
    line y = concat  . intersperseWrap "|" $ [elem (x, y) | x <- [0..size-1]]
    board = unlines . intersperseWrap separator . map line
    intersperseWrap s ss = s : intersperse s ss ++ [s]

go :: Int -> [Char]
go size = case solve size M.empty 1 (0, 0) of
    []    -> "No solution found"
    (s:_) -> printBoard size s

main = do
    args <- getArgs
    name <- getProgName
    putStrLn $ case map reads args of
        []             -> go 8
        [[(size, "")]] -> go size
        _              -> "Usage: " ++ name ++ " <size>"

-- solution 2 : using Warnsdorff's algorithm
type Square = (Int, Int)

-- Possible knight moves from a given square on an nxn board
knightMoves :: Int -> Square -> [Square]
knightMoves n (x, y) = filter (onBoard n)
        [(x+2, y+1), (x+2, y-1), (x+1, y+2), (x+1, y-2),
         (x-1, y+2), (x-1, y-2), (x-2, y+1), (x-2, y-1)]

-- Is the square within an nxn board?
onBoard :: Int -> Square -> Bool
onBoard n (x, y) = 1 <= x && x <= n && 1 <= y && y <= n

-- Knight's tours on an nxn board ending at the given square
knightsTo :: Int -> Square -> [[Square]]
knightsTo n finish = [pos:path | (pos, path) <- tour (n * n)]
  where tour 1 = [(finish, [])]
        tour k = [(pos', pos:path) |
                     (pos, path) <- tour (k - 1),
                     pos' <- sortImage (entrances path)  -- sort by positions with fewest onward moves
                             (filter (`notElem` path) (knightMoves n pos))]
        -- calculate the 'entrance' to (or from) a coordinate 'pos'
        entrances path pos =
                length (filter (`notElem` path) (knightMoves n pos))

-- Closed knight's tours on an nxn board
closedKnights :: Int -> [[Square]]
closedKnights n = [pos:path | (pos, path) <- tour (n*n), pos == start]
  where tour 1 = [(finish, [])]
        tour k = [(pos', pos:path) |
                (pos, path) <- tour (k-1),
                pos' <- sortImage (entrances path)
                        (filter (`notElem` path) (knightMoves n pos))]
        entrances path pos
          | pos == start = 100  -- don't visit start until there are no others
          | otherwise = length (filter (`notElem` path) (knightMoves n pos))
        start = (1,1)
        finish = (2,3)

-- Sort by comparing the image of list elements under a function f.
-- These images are saved to avoid recomputation.
sortImage :: Ord b => (a -> b) -> [a] -> [a]
sortImage f xs = map snd (sortBy cmpFst [(f x, x) | x <- xs])
  where cmpFst = comparing fst

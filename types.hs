module Shapes
( Shape(..)
, surface
) where

-- defining data type
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

:t Circle -- Circle :: Float -> Float -> Float -> Shape

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 x2 y1 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- calling
surface $ Circle 10 10 20 -- 314.15
surface $ Rectangle 0 0 100 100


-- more complex datatype
-- it's like defining member variables
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Int
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- type parameters
data Maybe a = Nothing | Just a


-- implementing trees
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- inserting elements into tree
nums = [8, 6, 4, 1, 7, 3, 5]
numTree = foldr treeInsert EmptyTree nums

-- type synonyms
type String = [Char]  -- String is a synonym for character list

type PhoneBook = [(String, String)]
type PhoneNumber = String
type Name = String

type AssocList k v = [(k, v)]  -- can take parameters
type IntMap v = Map Int v
type IntMap = Map Int
lookupAssocList :: (Eq k) => k -> AssocList k v -> Maybe v  -- example function with parameterized type


-- locker example
import qualified Data.Map as Map
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker Number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> 
      if state /= Taken 
      then Right code 
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"  

-- recursive definition
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
data List a = Empty | Cons { listHead :: a, listTail :: List a } deriving (Show, Read, Eq, Ord)

-- fixation
infixr 5 :-:  -- * is 7, + is 6
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
3 :-: 4 :-: 5 :-: Empty

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Monoids
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty  -- default definition == folding

-- example
instance Monoid [a] where
    mempthy = []
    mappend = (++)

-- another example
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

-- another
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- 'bind'
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- Monad!!
class Monad m where
    return :: a -> m a  -- just like 'pure'
    (>>=) :: m a -> (a -> m b) -> m b  -- 'bind'
    (>>) :: m a -> m b -> m b
    mx >> y = x >>= \_ -> y
    fail :: String -> m a
    fail msg = error msg

-- example
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

-- another example
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []

-- 'do' = chained 'bind's
-- below three statements are equal
[1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

  [ (n, ch) | n <- [1, 2], ch <- ['a', 'b'] ]

-- applyLog example
import Data.Monoid

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newlog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

("beans", Sum 10) `applyLog` addDrink -- ("milk", Sum {getSum = 35})
("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink -- ("beer", Sum {getSum = 65})


-- Writer
newtype Writer w a = Writer { runWriter :: (a, w) }
instance (Monad w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)

-- logging with Writer
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)

-- logging added
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)  -- for efficient list appending
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- diff list
f `append` g = \xs -> f (g xs)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))



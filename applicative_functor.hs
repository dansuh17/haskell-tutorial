-- functors are things that can be mapped
-- typeclass method fmap :: (a -> b) -> f a -> f b

instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)  -- f b is an IO action


main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

-- same program: here, IO's fmap would have type fmap :: (a -> b) -> IO a -> IO b
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line


-- famous functor is ((->) r)!
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))  -- mapping function over function == function composition!

-- first law of functors : fmpa id = id
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

-- second law of functors : fmap (f . g) = fmap f . fmap g

-- applicative functor = beefed-up functor
-- typeclass Applicative
-- in module Control.Applicative
class (Functor f) => Applicative f where
    pure :: a -> f a  -- take a single type and return applicative functor
    (<*>) :: f (a -> b) -> f a -> f b  -- takes the function inside of functor and mapps it over functor containing a

-- example - Maybe
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

-- this is how you use:
pure (+) <*> Just 3 <*> Just 5  -- Just 8
-- step 1: pure(+) <*> Just 3 ==> becomes Just (+3)
-- step 2: Just (+3) <*> Just 5 ==> Just 8

-- <$> is an infix version of fmap
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x


-- another applicative : []
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- another...
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)


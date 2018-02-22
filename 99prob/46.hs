-- define predicates and, or, nand, nor, xor, impl, equ
-- and write a predicate table that prints truth table
-- of given logical expression of two variables
not' :: Bool -> Bool
not' True = False
not' False = True

-- logical expressions implemented using Haskell primitives
and' = (&&)
or' = (||)
nand' = not . and'
nor' = not . or'
equ' = (==)
xor'= not . equ'
impl' a b = or' (not' a ) b

table :: (Bool -> Bool -> Bool) -> IO ()
-- use mapM_ which ignores intermediate results :: (a -> m b) -> t a -> m ()
table f = mapM_ putStrLn $ map (\(x, y) -> (show x) ++ " " ++ (show y) ++ " " ++ (show $ f x y)) combs
  where
    combs = [(x, y) | x <- bools, y <- bools]
    bools = [True, False]

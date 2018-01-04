-- flatten a nested structure
data NestedList a = Elem a | List [NestedList a]

-- myanswer
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List xs) = foldl (\a x -> a ++ (myFlatten x)) [] xs

-- using concatMap
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List a) = concatMap flatten x
-- concatMap :: (a -> [b]) -> [a] -> [b]  -- maps the function over a list and concatenates them

flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

flatten (Elem x) = return x
flatten (List x) = flatten =<< x  -- =<< :: Monad m => (a -> m b) -> m a -> m b

-- the above is same as:
flatten (Elem x) = return x
flatten (List x) = x >>= flatten  -- list's >>= operator concatenates all elements

-- making NestedList as instance of Foldable
import qualified Data.Foldable as F
instance F.Foldable NestedList where
    foldMap f (Elem x) = f x
    foldMap f (List []) = mempty
    foldMap f (List (x:xs)) = F.foldMap f x `mappend` F.foldMap f (List xs)
flatten' = F.foldMap (\x -> [a])

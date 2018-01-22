data ListItem a = Single a | Multiple Int a
  deriving (Show)

decodeHelper :: ListItem a -> [a]
decodeHelper (Single x) = [x]
decodeHelper (Multiple n x) = replicate n x

-- using foldl
decodeModified :: [ListItem a] -> [a]
decodeModified = foldl (\a x -> a ++ decodeHelper x) []

-- using concatMap
decodeModified2 :: [ListItem a] -> [a]
decodeModified2 = concatMap decodeHelper

-- using foldr
decodeModified3 :: [ListItem a] -> [a]
decodeModified3 = foldr func []
  where
    func (Single a) z = a:z
    func (Multiple n a) z = (replicate n a) ++ z

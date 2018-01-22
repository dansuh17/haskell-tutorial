--modiied run-length encoding
import Data.List (group)

-- requires utility type because Haskell's lists are homogeneous
data ListItem a = Single a | Multiple Int a
  deriving (Show)

-- using list comprehension
encodeModified2 :: Eq a => [a] -> [ListItem a]
encodeModified2 xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]

-- using pattern matching
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

-- a string representation of binary trees
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show)

-- make it work for any monads
stringToTree :: (Monad m) => String -> m (Tree Char)
stringToTree "" = return Empty
stringToTree [x] = return $ Branch x Empty Empty
stringToTree str = tfs str >>= \("", t) -> return t
  where tfs a@(x:xs)
          | x == ',' || x == ')' = return (a, Empty)  -- ex) (y,) or (,x)
        tfs (x:y:xs)
          | y == ',' || y == ')' = return (y:xs, Branch x Empty Empty)  -- (x,other) or (,x)
          -- this applies to all nested strings
          | y == '(' = do (',':xs', l) <- tfs xs
                          (')':xs'', r) <- tfs xs'  -- recursively find rignt branch
                          return (xs'', Branch x l r)
        tfs _ = fail "bad parse"

-- easily construct a string
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = [x] ++ "(" ++ (treeToString l) ++ "," ++ (treeToString r) ++ ")"

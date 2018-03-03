import Control.Monad (MonadPlus, mzero, mplus, guard, liftM, liftM2)
import Prelude hiding ((<*>))
import Data.Char (isSpace)
-- tree construction from a node string
data Tree a = Node a [Tree a] deriving (Show, Eq)

-- create a tree from string
-- the string is a sequence of characters in depth-first order.
-- in the sequence, a special character '^' is inserted whenever the move is a backtrack
-- during the tree traversal
stringToTree :: String -> Tree Char
stringToTree s = head $ fst $ stringToTreeH (stringToNodeHeights s 0) 0

-- a helper method - from a list of node element and its height, construct a list of tree
-- nodes having the same hight, i.e. sibling trees
stringToTreeH :: [(Char, Int)] -> Int -> ([Tree Char], [(Char, Int)])
stringToTreeH [] _ = ([], [])
stringToTreeH z@(x:xs) h
  | snd x == h = ([Node (fst x) childTrees] ++ siblingTrees, finalRem)
  | otherwise = ([], z)
  where
    (childTrees, rem) = stringToTreeH xs (h + 1)
    (siblingTrees, finalRem) = stringToTreeH rem h

stringToNodeHeights :: String -> Int -> [(Char, Int)]
stringToNodeHeights "" _ = []
stringToNodeHeights (x:xs) h
  | x == '^' = stringToNodeHeights xs (h - 1)
  | otherwise = (x, h):(stringToNodeHeights xs (h + 1))

treeToString :: Tree Char -> String
treeToString (Node x forest) = [x] ++ (concat $ map treeToString forest) ++ "^"

-- stringToTree "afg^^c^bd^e^^^" ==
-- Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]

-- other solutions:
-- 1. splitting the string with stack
stringToTreeStack :: String -> Tree Char
stringToTreeStack (x:xs@(y:ys))
  | y == '^' = Node x []
  | otherwise = Node x (map stringToTreeStack subs)
      -- try to split the string into list of strings representing subtrees
      -- "afg^^c^bd^e^^^" -> ["fg^^", "c^", "bd^e^^"]
      where subs = snd $ foldl parse ([], []) (init xs)
            parse ([], []) z = ([z], [[z]])
            parse (stack, acc) z = (stack', acc')
                where stack'
                        | z == '^' = init stack  -- pop
                        | otherwise = stack ++ [z]  -- push
                      acc' = if stack == []
                             then acc ++ [[z]]  -- start a new acc
                             else (init acc) ++ [(last acc) ++ [z]]

-- using a parser monad
-- newtype is like data with only one field and one constructor!!
-- a parser type P represents a function that accepts String and outputs Maybe (a, String)
newtype P a = P { runP :: String -> Maybe (a, String) }
-- an instance of Monad should define `return` and `bind`
instance Monad P where
  return x = P $ \s -> Just (x, s)
  -- (>>=) :: P a -> (a -> P b) -> P b
  -- f :: (a -> P b)
  -- v :: String -> Maybe (a, String)
  P v >>= f = P $ \s -> do
                     (x, s') <- v s  -- apply previous function v
                     runP (f x) s'  -- then apply f to output of previous function

instance MonadPlus P where
  mzero = P $ \_ -> Nothing  -- mzero :: P a
  P u `mplus` P v = P $ \s -> u s `mplus` v s  -- mplus :: P a -> P a -> P a

charP :: P Char
charP = P view_list
  where view_list [] = Nothing
        view_list (c:cs) = Just (c, cs)

literalP :: Char -> P ()
-- guard :: MonadPlus f => Bool -> f ()
-- return () if True, mzero if False
literalP c = do { c' <- charP; guard (c == c') }

-- erase out spaces
spaceP :: P ()
spaceP = P (\x -> Just ((), dropWhile isSpace x))

-- don't use class because we want multiple syntaxes for a given type
data Syntax a = Syntax {
  display :: a -> String,
  parse :: P a
}

-- concatenation
(<*>) :: Syntax a -> Syntax b -> Syntax (a, b)
a <*> b = Syntax {
  display = \(va, vb) -> display a va ++ display b vb,
  parse = liftM2 (,) (parse a) (parse b)
}

-- alternatives
(<|>) :: Syntax a -> Syntax b -> Syntax (Either a b)
a <|> b = Syntax {
  display = either (display a) (display b),
  parse = liftM Left (parse a) `mplus` liftM Right (parse b)
}

char :: Syntax Char
char = Syntax return charP

literal :: Char -> Syntax ()
literal c = Syntax (const [c]) (literalP c)

space :: Syntax ()
space = Syntax (const " ") spaceP

iso :: (a -> b) -> (b -> a) -> Syntax a -> Syntax b
iso a_to_b b_to_a a = Syntax {
                display = display a . b_to_a,
                parse = liftM a_to_b (parse a)
        }

-- concatenation, with no value in the first part
(*>) :: Syntax () -> Syntax a -> Syntax a
p *> q = iso snd ((,) ()) (p <*> q)

-- list of a's, followed by finish
list :: Syntax a -> Syntax () -> Syntax [a]
list a finish = iso toList fromList (finish <|> (a <*> list a finish))
  where toList (Left _) = []
        toList (Right (x, xs)) = x:xs
        fromList [] = Left ()
        fromList (x:xs) = Right (x, xs)

df :: Syntax (Tree Char)
df = iso toTree fromTree (char <*> list df (literal '^'))
  where toTree (x, ts) = Node x ts
        fromTree (Node x ts) = (x, ts)

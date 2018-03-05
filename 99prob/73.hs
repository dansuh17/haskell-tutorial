import Data.List (intercalate)
-- lisp-like tree representation
data Tree a = Node a [Tree a] deriving (Show, Eq)

lisp :: Tree Char -> String
lisp (Node x []) = [x]
lisp (Node x ts) = "(" ++ [x] ++ " " ++ (intercalate " " $ map lisp ts) ++ ")"

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

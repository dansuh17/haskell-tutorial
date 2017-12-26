-- case statement is similar to pattern matching
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

-- but not limited to defining functions
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- same thing using where clause
describeList2 :: [a] -> String
descrbieList2 xs = "The list is " ++ what xs
    where what [] = "empty"
          what [x] = "a singleton list"
          what xs = "a longer list"

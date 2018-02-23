-- gray codes : a sequence of n-bit strings constructed according to certain rules
gray :: Int -> [[Char]]
gray 1 = ["0", "1"]
gray n = ["0" ++ x | x <- gray (n - 1)] ++ ["1" ++ y | y <- reverse $ gray (n - 1)]

gray2 :: Int -> [String]
gray2 0 = [""]
gray2 n = let xs = gray (n - 1) in map ('0':) xs ++ map ('1':) (reverse xs)

gray3 :: Int -> [String]
gray3 0 = [""]
gray3 n = foldr (\s acc -> ("0" ++ s):acc ++ ["1" ++ s]) [] $ gray (n - 1)

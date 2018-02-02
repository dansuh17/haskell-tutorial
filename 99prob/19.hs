-- rotate a list by N to the left
myrotate :: [a] -> Int -> [a]
myrotate xs n = take len . drop (n `mod` len) . cycle $ xs
  where len = length xs

-- guards are like case-statements
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're normal"
    | bmi <= 30.0 = "You're fat!"
    | otherwise = "You're a whale"

-- using where
bmiTell2 :: (RealFloat a) => a -> a-> String
bmiTell2 weight height
    | bmi <= skinny = "You're underweight"
    | bmi <= normal = "You're normal"
    | bmi <= fat = "You're fat!"
    | otherwise = "You're a whale"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- pattern matching in where clause
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname


 -- defining function in where clause
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let <bindings> in <expr>
-- the expression after 'in' is evaluated and returned - hence, 'expression'
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- let in list comprehensions
calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
clacBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

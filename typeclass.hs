-- typeclasses are like interfaces - defines some behavior
-- typeclasses contain functions and their types
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)

-- this is how you make certain data types behave for typeclass
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow light"
  show Green = "Green Light"

-- typeclasses can be subclass to other typeclass
class (Eq a) => Num a where
  ...

-- Maybe has a type parameter -> declare it in 'instance'
instance Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False

-- however, since we're doing `x == y', it needs to be guaranteed that
-- m should be Eq!
instance (Eq m) => Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False

-- javascript-like boolean pattern
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True -- all numbers except 0 is true

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id  -- id returns the same thing!

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

-- then create a function having this constraint
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

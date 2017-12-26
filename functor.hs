-- 'f' is a type constructor that takes one param
-- NOT a concrete type
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- map function on list is an fmap! [] (Listtype) is a functor instance
instance Functor [] where
  fmap = map
map :: (a -> b) -> [a] -> [b]

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

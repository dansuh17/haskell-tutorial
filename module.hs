import Data.List (nub, sort)
import Data.List hiding (nub)  -- except nub
import qualified Data.List -- need to use functions like Data.List.nub
import qualified Data.List as M -- alias

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub  -- takes a list and removes duplicate element

-- in GHCI
-- ':m + Data.List' imports the modules

-- adding polynomials 3x^2 + 5x + 9, ...
map sum $ transpose [[0, 3, 5, 9], [10, 0, 0, 9], [8, 5, 1, -1]]
-- answer : [18, 8, 6, 17]


-- making your own modules
module Geometry
( sphereVolume
, spehereArea
, cubeVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)


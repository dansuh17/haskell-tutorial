module NinetyFive where

import Data.List (intersperse)

numberString = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

fullWords :: Int -> String
fullWords n = concat $ intersperse "-" strings
  where strings = [numberString !! i | i <- splitToDigits n]

splitToDigits :: Int -> [Int]
splitToDigits 0 = []
splitToDigits n = splitToDigits (n `div` 10) ++ [n `rem` 10]

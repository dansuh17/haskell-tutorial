module NinetySix where

import Data.Char (isLower, isDigit, isAlphaNum)

data Token = Hyphen | Digit | Letter deriving (Show, Eq)

isHyphen :: Char -> Bool
isHyphen c = c == '-'

getTokenType :: Char -> Token
getTokenType c
  | isHyphen c = Hyphen
  | isLower c = Letter
  | isDigit c = Digit
  | otherwise = error "cannot reach here"

-- syntax checker
-- see https://wiki.haskell.org/99_questions/95_to_99
identifier :: String -> Bool
identifier [] = False
identifier [c] = isLower c
identifier s =
  if isLower (head s) && isAlphaNum (last s)
  then snd $ checkLoop (init . tail $ s) Letter
  else False

checkLoop :: String -> Token -> (Token, Bool)
checkLoop [] _ = (Letter, True)
checkLoop s@(c:cs) tok
  | tok == Letter = checkLoop cs (getTokenType c)
  | tok == Digit = checkLoop cs (getTokenType c)
  | tok == Hyphen = if isHyphen c then (Hyphen, False) else checkLoop cs (getTokenType c)
  | otherwise = error "cannot reach here"

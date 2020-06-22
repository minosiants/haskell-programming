-- WarmingUp.hs

module WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

compose :: [Char] -> [Char]
compose = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tuppled :: [Char] -> ([Char], [Char])
tuppled = (,) <$> cap <*> rev

tuppledM :: [Char] -> ([Char], [Char])
tuppledM = do
  a <- cap
  b <- rev
  return (a, b)

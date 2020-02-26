-- recurtion.hs

module Recurtion where

applyTimes :: (Eq a, Num a) => 
  a -> (b -> b) -> b -> b

applyTimes 0 f b = b

applyTimes n f b = 
  f . applyTimes(n-1) f $ b

f :: Bool -> Int
f True = error "blah"
f False = 0

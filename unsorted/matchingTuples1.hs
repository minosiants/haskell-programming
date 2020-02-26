-- matchingTuples1.hs

module TupleFunctions where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) =  x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) ->  a
fst3 (x, _ , _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

ff::(a, b, c) -> (d, e, f) -> ((a, d), (c, f))
ff (a, _, c) (d, _, f) = ((a, d), (c, f))

functionC :: Ord a => a -> a -> a
functionC x y = if (x > y) then x else y


functionC' :: Ord a => a -> a -> a
functionC' x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

nums :: (Ord a, Num a, Num p) => a -> p
nums n = case compare n 0 of
  LT -> -1
  GT -> 1
  _  -> 0

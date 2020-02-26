-- partial.hs

module Partial where

helloStr :: String -> Int -> Int
helloStr a b =b



nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + (nonsense b) 

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i,b) = i + (nonsense b)

anonymus :: Integer -> Bool -> Integer
anonymus = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer 
anonNested = \i -> \b -> i + (nonsense b)


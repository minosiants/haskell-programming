-- greetIfCool.hs

module GreetIfCool where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
 if cool
  then putStrLn "eyyyyy. What's shakin"
 else putStrLn "pshhhh"
 where cool = 
        coolness == "downright frosty yo"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs::Integer -> Integer
myAbs x = if x > 0 then x else x - x - x

f::(a,b) -> (c,d) -> ((b,d), (a,c))
f a b = ((snd a, snd b), (fst a, fst b))

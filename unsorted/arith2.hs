-- arith2.hs

module Arith2 where 

add :: Int -> Int -> Int 
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do 
  print (0::Int)
  print (add 1 0)
  print (addOne 0)  
  print (addOnePF 0)
  print (( addOne . addOne ) 0)
  print ((addOnePF . addOne) 0)
  print ((addOne . addOnePF) 0)
  print (negate  (addOne 0))
  print ((negate . addOne) 0)
  print ((addOne . addOne . addOne . negate . addOne ) 0)

tensDigit :: Integral a => a -> a
tensDigit x  = fst (divMod x 10)

foldBool :: a-> a -> Bool -> a
foldBool x y b =
    case b of 
      True -> y
      otherwise -> x
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b 
    | b == True = y
    | otherwise = x

roundTrip :: (Show a , Read b) => a -> b 
roundTrip  = read . show

foo1 a = " 1 - " ++ a
foo2 a = " 2 - " ++ a
foo3 = foo1 . foo2

main2 :: IO ()
main2 = do
  print ((roundTrip 4)::Int)
  print (id 4)


  

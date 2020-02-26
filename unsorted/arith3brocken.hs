-- arith3brocken.hs

module Arith3Brocken where 

main :: IO ()
main = do 
  print (1 + 2) 
  putStrLn "10"
  print (negate (-1))
  print((+) 0 blah)
  where blah = negate (-1)


data Woot
data Blah

f::Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)

g (b,w) = (b, f w)


munge :: (x -> y)
      -> (y -> (w,z))
      -> x
      -> w
munge f1 f2 x = fst (f2 (f1 x))

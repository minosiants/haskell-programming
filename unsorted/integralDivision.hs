-- integralDivision.hs

module IntegralDivision where

 type Numerator = Integer
 type Denominator = Integer
 type Quotinent = Integer

 dividedBy :: Numerator
           -> Denominator
           -> Quotinent

 dividedBy = div

 dividedBy2 :: Integral a => a -> a -> (a, a)
 dividedBy2 num denom = go num denom 0
   where go n d count
           | n < d = (count , n)
           | otherwise = go (n-d) d (count + 1)


 sumNum :: (Eq a, Num a) => a -> a
 sumNum x = go x 0 0
   where go x count result
           | count == x = result + count
           | otherwise = go x (count +1) (result + count) 
 mult :: Integral a => a -> a -> a
 mult x y = go x y 0 0
   where go x y count result
           | count == y = result   
           | otherwise = go x y (count +1 ) (result + x)
                       

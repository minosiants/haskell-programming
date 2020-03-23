--Addition.hs

module Addition where 

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

devidedBy :: Integral a => a -> a -> (a, a)
devidedBy num denum = go num denum 0
    where go n d count 
            | n < d = (count, n)
            | otherwise =
                go (n-d) d (count +1)


multiply:: (Eq a, Num a) => a -> a -> a 
multiply x y = go x y 0 
    where go a b c
            | b == c = 0 
            | otherwise =  
                a + go a b (c+1)


trivialInt :: Gen Int
trivialInt = return 1

genBool :: Gen Bool
genBool = choose(True, False)

genBool' :: Gen Bool
genBool' = elements[False, True]

genOrdering :: Gen Ordering
genOrdering = elements[LT, EQ, GT]

genChar :: Gen Char
genChar = elements['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
          => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genTreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genTreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b)
          => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements[Left a, Right b]

genMaybe :: (Arbitrary a)
          => Gen (Maybe a)
genMaybe = do
  a <- arbitrary 
  elements[Nothing, Just a]

genMaybe' :: (Arbitrary a)
          => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

main :: IO ()
main = hspec $ do
      describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
          (1+1) > 1 `shouldBe` True
        it "2 + 2 is equal 4" $ do
          (2+2) `shouldBe` 4
        it "15 devided by 3 is 5" $ do
          devidedBy 15 3 `shouldBe` (5, 0) 
        it "22 devided by 5 is 4 reminder 2" $ do
          devidedBy 22 5 `shouldBe` (4, 2)
        it "3 multiplied by 5 is 15" $ do
          multiply 3 5 `shouldBe` 15
        it "x + 1 is always greater than 1" $ do
          property $ \x -> x+1 > (x::Int)

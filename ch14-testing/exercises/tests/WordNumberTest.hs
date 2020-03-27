-- WordNumberTest.hs

module Main where 
import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)


half :: Fractional a => a -> a
half x = x / 2

halfIdentity = (*2).half

prop_half :: Property
prop_half = 
   forAll  (arbitrary :: Gen Double)
        (\x ->  halfIdentity x == x)
prop_half' x = halfIdentity x == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = 
    snd $ foldr go (Nothing, True) xs
      where go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, t) = (Just y, x  >= y)
            
prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs = listOrdered (sort xs) == True

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool 
plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusAssociativeInt = plusAssociative::Int->Int->Int->Bool

prop_multiplication :: Int -> Int -> Int -> Bool
prop_multiplication x y z = x * ( y * z) == (x*y) * z

quotrem :: Int -> Int -> Bool
quotrem x y = (quot x y)*y + (rem x y) == x

genPositiveInt :: Gen Int
genPositiveInt = elements (take 100 $ iterate (+45) 1)

genIntPair :: Gen (Int, Int)
genIntPair = do
  a <- arbitrary :: Gen Int
  b <- genPositiveInt
  return (a, b)
    
prop_quotrem :: Property
prop_quotrem = forAll genIntPair 
      (\(x, y) -> quotrem x y)

divmod :: Int -> Int -> Bool
divmod x y = (div x y)*y + (mod x y) == x

prop_divmod :: Property
prop_divmod = forAll genIntPair
      (\(x, y) -> divmod x y)

prop_reverse :: Eq a =>  [a] -> Bool
prop_reverse xs = (reverse . reverse)  xs  == xs 


prop_application x = id $ x == id x 

listPairGen :: Arbitrary a => Gen ([a], [a]) 
listPairGen = do
    xs <- listOf arbitrary
    ys<- listOf arbitrary 
    return (xs, ys)

prop_listConcat :: Property
prop_listConcat = forAll (listPairGen::Gen([Int], [Int]))
    (\(xs, ys) -> foldr (:) ys xs ==  xs ++ ys)

prop_listConcat' :: Property
prop_listConcat' = forAll (listPairGen::Gen([String], [String]))
    (\(xs, _) -> foldr (++) [] xs == concat xs)

intGen :: Int -> Gen Int
intGen limit = choose (0, limit)

prop_length::Property
prop_length = forAll (do
    (xs, _) <- listPairGen::Gen([Int], [Int])
    n <- intGen(length xs)
    return (xs, n))
    (\(xs, n) -> length (take n xs) == n)

prop_readShow x = (read (show x)) == x

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord = fmap toUpper

idem :: String -> Bool
idem x = (capitalizeWord x == twice capitalizeWord x)
      && 
      (capitalizeWord x == fourTimes capitalizeWord x)

idem' :: [Integer] -> Bool
idem' xs = ( sort xs == twice sort xs)
            && 
            (sort xs == fourTimes sort xs)


data Fool = 
    Fools
  | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = frequency[(1, return Fools), (1, return Frue)]


foolGen' :: Gen Fool
foolGen' = frequency[(1, return Fools), (3, return Frue)]


main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1] 
    it "returns [1,0,0] for 100" $ do 
      digits 100 `shouldBe` [1, 0, 0]
  
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one given 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
  describe "quickcheck" $ do
    it "half" $ do
      quickCheck  prop_half'
    it "list" $ do
      quickCheck (prop_listOrdered::[Int] -> Bool)
    it "plusAssociative" $ do
      quickCheck prop_plusAssociativeInt
    it "multiplication" $ do
      quickCheck prop_multiplication
    it "quotrem" $ do
      quickCheck prop_quotrem
    it "divmod" $ do
      quickCheck prop_divmod
    it "reverse" $ do
      quickCheck (prop_reverse::[Int] -> Bool)
    it "application" $ do
      quickCheck (prop_application::Int -> Bool)
    it "concat" $ do
      quickCheck prop_listConcat
    it "concat'" $ do
      quickCheck prop_listConcat'
    it "length" $ do
      quickCheck prop_length
    it "readShow" $ do
      quickCheck (prop_readShow::Int -> Bool)
    it "idem" $ do
      quickCheck idem
    it "idem'" $ do
      quickCheck idem'

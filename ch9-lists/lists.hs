-- lsits.hs

module Lists where 

import Data.Char

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

myWords :: Char -> String -> [String]
myWords sep [] = []
myWords sep x = 
   case dropWhile (/= sep) x of
       "" -> [x]
       (sep: az) -> (takeWhile (/= sep) x) : (myWords sep az) 



firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"


sentences = firstSen ++ secondSen ++ thirdSen
            ++ fourthSen

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = 
    (x, y) : myZip xs ys


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) =
     f x y : myZipWith f xs ys


myZip2 :: [a] -> [b] -> [(a,b)]
myZip2 xs ys = myZipWith (\x y -> (x,y)) xs ys

filterUpper :: String -> String
filterUpper str = filter isUpper str

cappitalize :: String -> String
cappitalize (x:xs) = toUpper x : xs

cappitalize2 :: String -> String
cappitalize2 [] =[]
cappitalize2 (x:xs) = toUpper x : cappitalize2 xs

cappitalize3 :: String -> Char 
cappitalize3 (x:xs) = head $ toUpper x : xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs) = (el == x) || myElem el xs


myElem2 :: Eq a => a -> [a] -> Bool
myElem2 _ [] = False
myElem2 el xs = any (== el) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


squish :: [[a]] -> [a]
squish  [] = []
squish (x:xs) = x ++ squish xs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> x) xs

myBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myBy ord f (x:xs) = go x xs
       where
          go x' [] = x'
          go x' (x'':xs)
             | f x' x'' == ord = go x' xs
             | otherwise = go x'' xs


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = myBy GT f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = myBy LT f xs


myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs 

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs

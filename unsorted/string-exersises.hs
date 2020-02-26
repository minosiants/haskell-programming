-- string-exersises.hs


module StringExersises where 

thirdLetter :: String ->  Char
thirdLetter x = x !! 3


letterIndex :: Int -> Char
letterIndex i = 
 "Curry is awersome !" !! i

rvrs :: String -> String
rvrs s = awersome ++ " " ++ is ++ " " ++ curry ++ excl where
  awersome = take 8 (drop 9 s)
  is = take 2 (drop 6 s)
  curry = take 5 s
  excl = drop 18 s


main :: IO ()
main = print $ rvrs "curry is awersome !"


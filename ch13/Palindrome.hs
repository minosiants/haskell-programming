-- Palindrome.hs

module Palindrome where

import System.Exit (exitSuccess)
import Control.Monad 
import System.IO (BufferMode(NoBuffering), 
                  hSetBuffering,
                  stdout)
import Data.Char (toLower)



deleteChar :: Char -> [Char] -> [Char]
deleteChar ch = foldr (\a b -> 
    if a == ch then b else a:b) [] 


palindrome :: IO ()
palindrome = forever $ do 
    line1 <- getLine
    let line1' = fmap toLower $ deleteChar '\'' line1 
    case (line1'  == reverse line1' ) of
      True -> putStrLn "It is palindrome"
      False -> exitSuccess 


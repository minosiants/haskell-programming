-- sting.hs

module Sting where 

firstString :: [Char] -> [Char]
firstString a  = a++ "in the rain" 

sndString :: [Char] -> [Char]
sndString x = x ++ "over the rainbow"

sting = if (x > y) then firstString x else sndString y 
 where x = "Stingin"
       y = "Somewhere"

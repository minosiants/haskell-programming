-- LearnParsers.hs

module LearnParsers where

import Control.Applicative
import Text.Trifecta

stop :: Parser ()
stop = unexpected "stop"

one = char '1'

one' = one >> stop

one'' = one >> eof

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

p123 :: Parser String
p123 = string "123" <|> string "12" <|> string "1"

parseInt :: Parser Integer
parseInt = integer <* eof

testParse :: Show p => Parser p -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "p123"
  testParse p123
  pNL "parseInt"
  testParse parseInt 

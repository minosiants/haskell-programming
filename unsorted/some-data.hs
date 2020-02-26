-- some-data.hs

module SomeData where

data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood _ = Blah

-- HttpStuff.hs

module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls =
  [ "http://httpbin.org/ip",
    "http://httpbin.org/bytes/5"
  ]

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

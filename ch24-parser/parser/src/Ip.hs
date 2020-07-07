-- Ip.hs

module Ip where

import Control.Applicative
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.List
import Data.Word
import Debug.Trace
import Numeric (readHex)
import Text.Trifecta

data IPv4 = IPv4 Word32 deriving (Eq, Show)

parseIpv4Part :: Parser Word8
parseIpv4Part = read <$> manyTill digit (dot *> return () <|> eof)

parseIpv4Parts :: Parser [Word8]
parseIpv4Parts = count 4 (parseIpv4Part <* skipMany dot)

ipv4Parcer :: Parser IPv4
ipv4Parcer = IPv4 <$> conv <$> parseIpv4Parts
  where
    conv :: [Word8] -> Word32
    conv = foldl (\acc b -> (acc `shiftL` 8) .|. (fromIntegral b)) 0

data IPv6
  = IPv6 Word64 Word64
  deriving (Eq, Ord, Show)

parseIpv6Part :: Parser Word16
parseIpv6Part = do
  res <- manyTill hexDigit (colon *> return () <|> eof)
  return $ case res of
    [] -> 0
    xs ->
      fst . head $ readHex (xs)

parseIpv6Parts :: Parser [Word16]
parseIpv6Parts = count 8 (parseIpv6Part)

parseIpv6 :: Parser IPv6
parseIpv6 = do
  (a, b) <- conv <$> parseIpv6Parts
  return $ IPv6 a b
  where
    conv parts =
      let (a, b) = splitAt 4 parts
       in (toWord64 a, toWord64 b)
    toWord64 =
      foldl (\acc p -> acc `shiftL` 16 .|. (fromIntegral p)) 0

readPart p = parseString parseIpv6Part mempty p

main :: IO ()
main = do
  print $ parseString ipv4Parcer mempty "172.16.254.1"
  print $
    parseString
      parseIpv6
      mempty
      "2001:DB8::8:800:200C:417A"

-- Ip.hs

module Ip where

import Control.Applicative
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import Data.Word
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



data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)


main :: IO ()
main = do
  print $ parseString ipv4Parcer mempty "172.16.254.1"


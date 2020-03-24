-- Naturals.hs

module Naturals where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat i 
      | i < 0 = Nothing
      | otherwise = 
          fmap Succ $ integerToNat (i-1)

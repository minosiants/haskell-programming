module DList where

import Criterion.Main

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL $ const []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL $ const [x]
{-# INLINE singleton #-}

-- Prepend a single element  to a dlist
infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)

--  {-# INLINE cons #-}

-- Append a single element to a dlist.
infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = DL ((unDL xs) ++ [x])

--  {-# INLINE snoc #-}

-- Spprnf dlists.
append :: DList a -> DList a -> DList a
append xs ys = DL ((unDL xs) ++ (unDL ys))

--  {-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n -1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs =
      go
        (n -1)
        (singleton n `append` xs)

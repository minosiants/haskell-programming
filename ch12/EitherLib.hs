--EitherLisb.hs

module EitherLib where

lefts' :: [Either a b] -> [a]
lefts'  = foldr 
            (\a b -> 
                case a of
                    Left l -> l:b
                    Right _ -> b) []
                
                  

rights' :: [Either a b] -> [b]
rights' = foldr 
            (\a b -> 
                case a of
                    Left _ -> b
                    Right r -> r:b) []
                
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) 
             -> Either a b
             -> Maybe c 
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right r) = Just $ f r

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left l) = f l
either' _ f (Right r) = f r

eitherMaybe'' :: (b -> c) 
              -> Either a b
              -> Maybe c
eitherMaybe'' f e = 
    either' (\a -> Nothing)  (\b -> Just (f b)) e


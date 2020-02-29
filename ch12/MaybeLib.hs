-- MaybeLib.hs

module MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just v) = f v
mayybee n _ Nothing = n

fromMaybe :: a -> Maybe a -> a
fromMaybe v m = mayybee v id m

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybe :: [Maybe a] -> [a]
catMaybe [] = []
catMaybe (x:xs) = maybeToList x ++ catMaybe xs 

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
    | length catted < length xs = Nothing
    | otherwise = Just catted
    where 
      catted = catMaybe xs

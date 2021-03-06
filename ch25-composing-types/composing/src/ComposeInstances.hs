{-# LANGUAGE InstanceSigs #-}

module ComposeInstances where

newtype Identity a
  = Identity {runIdentity :: a}

newtype Compose f g a
  = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

-- Lifting over

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance
  (Applicative f, Applicative g) =>
  Applicative (Compose f g)
  where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fs) <*> (Compose xs) = Compose $ (<*>) <$> fs <*> xs

instance
  (Foldable f, Foldable g) =>
  Foldable (Compose f g)
  where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance
  (Traversable f, Traversable g) =>
  Traversable (Compose f g)
  where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

main :: IO ()
main = do
  putStrLn "hello world"

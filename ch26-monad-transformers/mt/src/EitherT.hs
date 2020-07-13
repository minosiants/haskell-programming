-- EitherT.hs

module EitherT where

newtype EitherT e m a
  = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT em) = EitherT $ (fmap . fmap) f em

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  (EitherT f) <*> (EitherT ma) = EitherT $ (<*>) <$> f <*> ma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) (EitherT ema) f = EitherT $ do
    v <- ema
    case v of
      (Left e) -> return $ Left e
      (Right a) -> runEitherT (f a)

swapEither :: Either a b -> Either b a
swapEither = either Right Left

swapEitherT ::
  (Functor m) =>
  EitherT e m a ->
  EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT

eitherT ::
  Monad m =>
  (a -> m c) ->
  (b -> m c) ->
  EitherT a m b ->
  m c
eitherT fa fb t =
  (runEitherT t) >>= (either fa fb)

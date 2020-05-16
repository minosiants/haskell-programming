-- Bind.hs
module Bind where
import Control.Monad ( join)

bind:: Monad m => (a -> m b) -> m a -> m b
bind f fa = join ( fmap f fa )

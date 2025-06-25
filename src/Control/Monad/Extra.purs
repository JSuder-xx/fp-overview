module Control.Monad.Extra where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe)

onJust :: forall m a. Monad m => m (Maybe a) -> (a -> m Unit) -> m Unit
onJust m' f = m' >>= traverse_ f

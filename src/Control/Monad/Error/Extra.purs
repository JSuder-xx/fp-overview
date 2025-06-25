module Control.Monad.Error.Extra where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Maybe (Maybe, maybe)

assertJust :: forall m e a. MonadThrow e m => e -> Maybe a -> m a
assertJust error = maybe (throwError error) pure

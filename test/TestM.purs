module Test.TestM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Error.Extra (assertJust)
import Control.Monad.State (class MonadState, get, modify_)
import Data.Array as Array
import Data.Lens (Lens')
import Data.Lens as Lens

-- | A convenience function for test monads. Often there will be some array of values that should be consumed during the test.
takeHead :: forall m error a state. MonadThrow error m => MonadState state m => error -> Lens' state (Array a) -> m a
takeHead error _values = do
  oldState <- get
  { head, tail } <- assertJust error $ Array.uncons $ Lens.view _values oldState
  modify_ $ Lens.set _values tail
  pure head

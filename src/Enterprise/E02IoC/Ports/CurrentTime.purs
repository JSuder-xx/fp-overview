module Enterprise.E02IoC.Ports.CurrentTime where

import Prelude
import Data.DateTime (DateTime)

-- | An ABSTRACT representation of a monad that provides the current time.
-- |
-- | When unit testing this can either be a hardcoded value, a constant value provided by MonadAsk (where the time is stored in a configuration),
-- | or the current time could be stored in a State type and every time a current time is read a value is popped off a stack / queue of pre-configured
-- | date/times.
class Monad m <= CurrentTime m where
  currentTime :: m DateTime

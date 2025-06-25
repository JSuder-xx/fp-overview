module Enterprise.Exception where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | Engineers often spend a little too much time worrying about an exception taxonomy. In some
-- | cases you want details exceptions BUT generally detailed exceptions go against the very notion
-- | of avoiding exceptions as flow control.
data Exception
  = SystemErrorMessage { internalErrorMessage :: String }
  | RetryableSystemError { userErrorMessage :: String }

derive instance Eq Exception
derive instance Ord Exception
derive instance Generic Exception _

instance Show Exception where
  show = genericShow

toString :: Exception -> String
toString = case _ of
  SystemErrorMessage { internalErrorMessage } -> "SYSTEM ERROR: " <> internalErrorMessage
  RetryableSystemError { userErrorMessage } -> "RETRY-ABLE ERROR: " <> userErrorMessage

-- | Gives the user an option to retry if a retry-able error is thrown.
promptyForRetry :: forall (m :: Type -> Type) (a :: Type). MonadError Exception m => (String -> m Boolean) -> m a -> m a
promptyForRetry prompt monadicComputation = catchError monadicComputation catch
  where
  catch exception = case exception of
    SystemErrorMessage _ -> throwError exception
    RetryableSystemError { userErrorMessage } -> do
      shouldRetry <- prompt userErrorMessage
      if shouldRetry then promptyForRetry prompt monadicComputation
      else throwError $ SystemErrorMessage { internalErrorMessage: userErrorMessage }

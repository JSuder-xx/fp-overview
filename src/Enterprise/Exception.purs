module Enterprise.Exception where

import Prelude

-- | Engineers often spend a little too much time worrying about an exception taxonomy. In some
-- | cases you want details exceptions BUT generally detailed exceptions go against the very notion
-- | of avoiding exceptions as flow control.
data Exception
  = SystemErrorMessage { internalErrorMessage :: String }
  | RetryableSystemError { userErrorMessage :: String }

toString :: Exception -> String
toString = case _ of
  SystemErrorMessage { internalErrorMessage } -> "SYSTEM ERROR: " <> internalErrorMessage
  RetryableSystemError { userErrorMessage } -> "RETRY-ABLE ERROR: " <> userErrorMessage

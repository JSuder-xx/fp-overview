module Enterprise.TransactionError where

data TransactionError = InsufficientFunds | UnableToInsureBalance

display :: TransactionError -> String
display = case _ of
  InsufficientFunds -> "Insufficient Funds"
  UnableToInsureBalance -> "Unable to Insure Balance"

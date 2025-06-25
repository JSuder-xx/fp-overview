module Enterprise.TransactionError where

data TransactionError = InsufficientFunds | UnableToInsureBalance

display :: TransactionError -> String
display = case _ of
  InsufficientFunds -> "Insufficient funds"
  UnableToInsureBalance -> "Unable to insure balance"

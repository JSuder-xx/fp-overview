module Enterprise.Transaction where

import Data.DateTime (DateTime)
import Enterprise.USCents (USCents, zeroCents, ($-))

data Transaction
  = Deposit { when :: DateTime, amount :: USCents }
  | Withdrawal { when :: DateTime, amount :: USCents }

netEffectOnBalance :: Transaction -> USCents
netEffectOnBalance = case _ of
  Deposit { amount } -> amount
  Withdrawal { amount } -> zeroCents $- amount

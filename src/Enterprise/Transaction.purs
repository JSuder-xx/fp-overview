module Enterprise.Transaction where

import Prelude

import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Enterprise.USCents (USCents, zeroCents, ($-))

data Transaction
  = Deposit { when :: DateTime, amount :: USCents }
  | Withdrawal { when :: DateTime, amount :: USCents }

derive instance Eq Transaction
derive instance Generic Transaction _

instance Show Transaction where
  show = genericShow

netEffectOnBalance :: Transaction -> USCents
netEffectOnBalance = case _ of
  Deposit { amount } -> amount
  Withdrawal { amount } -> zeroCents $- amount

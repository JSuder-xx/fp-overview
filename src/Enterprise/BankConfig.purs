module Enterprise.BankConfig where

import Enterprise.USCents (USCents)

type BankConfig = { maximumInsurableBalance :: USCents }

type HasBankConfig r = { bankConfig :: BankConfig | r }

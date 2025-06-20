module Enterprise.E02IoC.Ports.BankAccountRepository where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Enterprise.E02IoC.Entity.BankAccount (BankAccount, BankAccountId)
import Enterprise.Exception (Exception)

class MonadThrow Exception m <= BankAccountRepository m where
  loadBankAccount :: BankAccountId -> m BankAccount
  saveBankAccount :: BankAccount -> m Unit

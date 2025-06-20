module Enterprise.E02IoC.Entity.BankAccount
  ( BankAccountId(..)
  , BankAccount -- OBSERVE that the constructor is not exposed. This is what is called an Opaque Data type.
  , _name
  , deposit
  , id
  , mkBankAccount
  , total
  , transactions
  , withdraw
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (lift, runExceptT)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.String.NonEmpty (NonEmptyString)
import Data.UUID (UUID)
import Enterprise.BankConfig (HasBankConfig)
import Enterprise.E02IoC.Ports.CurrentTime (class CurrentTime, currentTime)
import Enterprise.Transaction (Transaction(..))
import Enterprise.Transaction as Transaction
import Enterprise.TransactionError (TransactionError)
import Enterprise.TransactionError as TransactionError
import Enterprise.USCents (USCents, ($+))

newtype BankAccountId = BankAccountId UUID

derive instance Eq BankAccountId
derive instance Ord BankAccountId
derive newtype instance Show BankAccountId

newtype BankAccount = BankAccount BankAccountRecord
type BankAccountRecord =
  { id :: BankAccountId
  , name :: NonEmptyString
  , transactions :: Array Transaction
  }

mkBankAccount :: BankAccountId -> { name :: NonEmptyString } -> BankAccount
mkBankAccount id' { name } =
  BankAccount { id: id', name, transactions: [] }

id :: BankAccount -> BankAccountId
id = _.id <<< bankAccountRecord

transactions :: BankAccount -> Array Transaction
transactions = _.transactions <<< bankAccountRecord

_name :: Lens' BankAccount NonEmptyString
_name = _bankAccountRecord <<< Lens.lens _.name _ { name = _ }

total :: BankAccount -> USCents
total = transactions >>> foldMap Transaction.netEffectOnBalance

withdraw
  :: forall m
   . CurrentTime m
  => USCents
  -> BankAccount
  -> m (Either TransactionError BankAccount)
withdraw amount bankAccount =
  if total bankAccount < amount then pure $ Left TransactionError.InsufficientFunds
  else currentTime <#> \when -> Right $ Lens.over _transactions (Array.cons $ Withdrawal { when, amount }) bankAccount

deposit
  :: forall m rOtherConfig
   . CurrentTime m
  => MonadAsk (HasBankConfig rOtherConfig) m
  => USCents
  -> BankAccount
  -> m (Either TransactionError BankAccount)
deposit depositAmount bankAccount = runExceptT do
  { bankConfig: { maximumInsurableBalance } } <- ask
  when (total bankAccount $+ depositAmount > maximumInsurableBalance) $
    throwError TransactionError.UnableToInsureBalance
  when <- lift currentTime
  pure $ Lens.over _transactions (Array.cons $ Deposit { when, amount: depositAmount }) bankAccount

---------------------------------------------------------------------------------
-- Private
---------------------------------------------------------------------------------
bankAccountRecord :: BankAccount -> BankAccountRecord
bankAccountRecord (BankAccount r) = r

_bankAccountRecord :: Lens' BankAccount BankAccountRecord
_bankAccountRecord = Lens.lens bankAccountRecord \_ -> BankAccount

_transactions :: Lens' BankAccount (Array Transaction)
_transactions = _bankAccountRecord <<< Lens.lens _.transactions _ { transactions = _ }

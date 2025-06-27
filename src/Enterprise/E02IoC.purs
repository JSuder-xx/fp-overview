module Enterprise.E02IoC where

-- This is the top level module showing off a mini-program that runs with controlled effects using
-- a compile-time version of Inversion of Control. All of the computations here rely on abstractions
-- not concretions. This is effectively a hexagonal approach (ports-and-adapters).

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Extra (onJust)
import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either(..), note)
import Data.Lens as Lens
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NonEmptyString
import Enterprise.BankConfig (HasBankConfig)
import Enterprise.E02IoC.Entity.BankAccount (BankAccount, BankAccountId)
import Enterprise.E02IoC.Entity.BankAccount as BankAccount
import Enterprise.E02IoC.Ports.BankAccountRepository (class BankAccountRepository, loadBankAccount, saveBankAccount)
import Enterprise.E02IoC.Ports.Console (class Console)
import Enterprise.E02IoC.Ports.Console as Console
import Enterprise.E02IoC.Ports.CurrentTime (class CurrentTime)
import Enterprise.Exception (Exception, promptyForRetry)
import Enterprise.TransactionError as TransactionError
import Enterprise.USCents as USCents

-- | This Manage Bank Account mini program driver has completely controlled effects. That means that it is IMPOSSIBLE
-- | for this function to do anything except what is listed for the capabilities. This function may
-- | - `BankAccountRepository` may read/write bank accounts from persistent storage.
-- | - `MonadAsk` may read some configuration when it is finally run.
-- | - `CurrentTime` may read the current time. HOWEVER, it is doing so through `CurrentTime` which means that it is not getting the time from anywhere else.
-- | - `MonadError` may throw an error.
-- | - `Console` may read/write text from and to a virtual console. This could be implemented one way for tests, one way for a NodeJS application
-- | and another way for a web browser that displayed a Text area for output and a single input for collecting readLn's.
manageBankAccount
  :: forall m rConfig
   . BankAccountRepository m
  => MonadAsk (HasBankConfig rConfig) m
  => CurrentTime m
  => MonadError Exception m
  => Console m
  => BankAccountId
  -> m Unit
manageBankAccount bankAccountId = do
  bankAccount <- loadBankAccount bankAccountId
    # promptyForRetry \errorMessage -> do
        Console.writeLn $ "Error: " <> errorMessage
        Console.promptYesNo { default: true } "Would you like to retry?"
  manageBankAccount' bankAccount
  where
  manageBankAccount' :: BankAccount -> m Unit
  manageBankAccount' bankAccount = do
    Console.writeLn ""
    Console.writeLn $ "What would you like to do today with account '" <> BankAccount.name bankAccount <> "'?"
    Console.displayOptions operationOptions
    choice <- Console.prompt { default: Just Quit } operationOptions ""
    case choice of
      Quit -> Console.writeLn "Bye!"
      CheckBalance -> do
        Console.writeLn $ "Current Balance: " <> (USCents.format $ BankAccount.balance bankAccount)
        manageBankAccount' bankAccount
      WithdrawFunds -> executeBalanceOperation BankAccount.withdraw "How much would you like to withdraw?"
      DepositFunds -> executeBalanceOperation BankAccount.deposit "How much would you like to deposit?"
      ChangeName -> do
        Console.writeLn $ "The current name is '" <> BankAccount.name bankAccount <> "'. What would you like it to be?"
        onJust (Console.readLnParsed $ note "Must be non-empty" <<< NonEmptyString.fromString) \name ->
          bankAccount
            # Lens.set BankAccount._name name
            # saveBankAccount
            >>= manageBankAccount'
    where
    operationOptions =
      [ { char: '1', description: "Check Balance", value: CheckBalance }
      , { char: '2', description: "Withdraw Funds", value: WithdrawFunds }
      , { char: '3', description: "Deposit Funds", value: DepositFunds }
      , { char: '4', description: "Change Account Name", value: DepositFunds }
      , { char: '5', description: "Quit", value: Quit }
      ]

    executeBalanceOperation op prompt = do
      Console.writeLn prompt
      onJust (Console.readLnParsed USCents.parse) \amount -> do
        result' <- op amount bankAccount
        case result' of
          Left err -> do
            Console.writeLn $ TransactionError.display err
            manageBankAccount' bankAccount
          Right newBankAccount -> do
            void $ saveBankAccount newBankAccount
            Console.writeLn $ "The new balance is " <> (USCents.format $ BankAccount.balance newBankAccount)
            manageBankAccount' newBankAccount

data Operation = CheckBalance | WithdrawFunds | DepositFunds | ChangeName | Quit

derive instance Eq Operation

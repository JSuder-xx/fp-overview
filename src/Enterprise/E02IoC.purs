module Enterprise.E02IoC where

-- This is the top level module showing off a mini-program that runs with controlled effects using
-- a compile-time version of Inversion of Control. All of the computations here rely on abstractions
-- not concretions. This is effectively a hexagonal approach (ports-and-adapters).

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Lens as Lens
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NonEmptyString
import Enterprise.BankConfig (HasBankConfig)
import Enterprise.E02IoC.Entity.BankAccount (BankAccount, BankAccountId)
import Enterprise.E02IoC.Entity.BankAccount as BankAccount
import Enterprise.E02IoC.Ports.BankAccountRepository (class BankAccountRepository, loadBankAccount, saveBankAccount)
import Enterprise.E02IoC.Ports.Console (class Console, promptYesNo, readLnParsed, selection, writeLn)
import Enterprise.E02IoC.Ports.CurrentTime (class CurrentTime)
import Enterprise.Exception (Exception(..))
import Enterprise.TransactionError as TransactionError
import Enterprise.USCents (USCents)
import Enterprise.USCents as USCents

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
  bankAccount <- loadBankAccount'
  manageBankAccount' bankAccount
  where
  loadBankAccount' = catchError (loadBankAccount bankAccountId) catch
    where
    catch :: Exception -> m BankAccount
    catch exception = case exception of
      SystemErrorMessage _ -> throwError exception
      RetryableSystemError { userErrorMessage } -> do
        writeLn $ "Error: " <> userErrorMessage
        shouldRetry <- promptYesNo { default: true } "Would you like to retry?"
        if shouldRetry then loadBankAccount'
        else throwError $ SystemErrorMessage { internalErrorMessage: userErrorMessage }

  manageBankAccount' :: BankAccount -> m Unit
  manageBankAccount' bankAccount = do
    writeLn ""
    choice <-
      selection { default: Just Quit }
        [ { char: '1', description: "Check Balance", value: CheckBalance }
        , { char: '2', description: "Withdraw Funds", value: WithdrawFunds }
        , { char: '3', description: "Deposit Funds", value: DepositFunds }
        , { char: '4', description: "Quit", value: Quit }
        ]
        $ fold
            [ "What would you like to do today with account '"
            , NonEmptyString.toString $ Lens.view BankAccount._name bankAccount
            , "'?"
            ]
    case choice of
      CheckBalance -> do
        writeLn $ "Current Balance: " <> (USCents.format $ BankAccount.total bankAccount)
        manageBankAccount' bankAccount
      WithdrawFunds -> do
        writeLn "How much would you like to withdraw?"
        amountMaybe :: Maybe USCents <- readLnParsed USCents.parse
        for_ amountMaybe \amount -> do
          result' <- BankAccount.withdraw amount bankAccount
          processBankResult result'
      DepositFunds -> do
        writeLn "How much would you like to deposit?"
        amountMaybe :: Maybe USCents <- readLnParsed USCents.parse
        for_ amountMaybe \amount -> do
          result' <- BankAccount.deposit amount bankAccount
          processBankResult result'
      Quit ->
        writeLn "Bye!"
    where
    processBankResult = case _ of
      Left err -> do
        writeLn $ TransactionError.display err
        manageBankAccount' bankAccount
      Right newBankAccount -> do
        saveBankAccount newBankAccount
        writeLn $ "The new balance is " <> (USCents.format $ BankAccount.total newBankAccount)
        manageBankAccount' newBankAccount

data Operation = CheckBalance | WithdrawFunds | DepositFunds | Quit

derive instance Eq Operation

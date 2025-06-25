module Main where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, liftEither)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.UUID (emptyUUID)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import Effect.Ref (Ref, read, write)
import Effect.Ref as Ref
import Enterprise.BankConfig (HasBankConfig)
import Enterprise.E02IoC (manageBankAccount)
import Enterprise.E02IoC.Entity.BankAccount (BankAccount, BankAccountId(..))
import Enterprise.E02IoC.Entity.BankAccount as BankAccount
import Enterprise.E02IoC.Ports.BankAccountRepository (class BankAccountRepository)
import Enterprise.E02IoC.Ports.Console (class Console)
import Enterprise.E02IoC.Ports.CurrentTime (class CurrentTime)
import Enterprise.Exception (Exception(..))
import Enterprise.Exception as Exception
import Enterprise.USCents (USCents(..))
import Node.ReadLine as ReadLine
import Node.ReadLine.Aff as ReadLine.Aff

type Config = HasBankConfig
  ( bankAccountsRef :: Ref (Map BankAccountId BankAccount)
  , consoleInterface :: ReadLine.Interface
  )

-- This is what is known as an Application Monad Stack. It defines the real world implementation of various "capaibilities" (interfaces or Ports).
-- This Monad Stack can have effects. This is the Adapter.
newtype AppM a = AppM (ReaderT Config (ExceptT Exception Aff) a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadAsk Config AppM
derive newtype instance MonadThrow Exception AppM
derive newtype instance MonadError Exception AppM

instance CurrentTime AppM where
  currentTime = liftEffect nowDateTime

instance BankAccountRepository AppM where
  loadBankAccount bankAccountId = do
    { bankAccountsRef } <- ask
    bankAccounts <- liftEffect $ read bankAccountsRef
    case Map.lookup bankAccountId bankAccounts of
      Nothing -> throwError $ SystemErrorMessage { internalErrorMessage: "Unable to find bank account " <> show bankAccountId }
      Just bankAccount -> pure bankAccount

  saveBankAccount bankAccount = do
    { bankAccountsRef } <- ask
    bankAccounts <- liftEffect $ read bankAccountsRef
    liftEffect $ write (Map.insert (BankAccount.id bankAccount) bankAccount bankAccounts) bankAccountsRef
    pure bankAccount

instance Console AppM where
  writeLn = log
  readLn = do
    { consoleInterface } <- ask
    liftAff $ ReadLine.Aff.question "" consoleInterface

mkConfig :: Map BankAccountId BankAccount -> Effect Config
mkConfig initialBanks = do
  consoleInterface <- ReadLine.createConsoleInterface (\_ -> pure mempty)
  bankAccountsRef <- Ref.new initialBanks
  pure
    { bankConfig: { maximumInsurableBalance: USCents 50_000_00 }
    , consoleInterface
    , bankAccountsRef
    }

runAppM :: Config -> AppM ~> Aff
runAppM config (AppM monad) = do
  result' <- runExceptT (runReaderT monad config)
  liftEither $ lmap (error <<< Exception.toString) result'

main :: Effect Unit
main = do
  config <- mkConfig $ Map.singleton bankAccountId initialBankAccount
  launchAff_ $ runAppM config $ manageBankAccount bankAccountId
  where
  initialBankAccount = BankAccount.mkBankAccount bankAccountId { name: NonEmptyString "John's Account" }
  bankAccountId = BankAccountId emptyUUID

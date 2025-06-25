module Test.Enterprise.E02IoC where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Error.Extra (assertJust)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.State (class MonadState, State, get, modify_, runState)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.JSDate as JSDate
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Data.UUID (emptyUUID)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Enterprise.BankConfig (BankConfig, HasBankConfig)
import Enterprise.E02IoC as E02IoC
import Enterprise.E02IoC.Entity.BankAccount (BankAccount, BankAccountId(..))
import Enterprise.E02IoC.Entity.BankAccount as BankAccount
import Enterprise.E02IoC.Ports.BankAccountRepository (class BankAccountRepository)
import Enterprise.E02IoC.Ports.Console (class Console)
import Enterprise.E02IoC.Ports.CurrentTime (class CurrentTime)
import Enterprise.Exception (Exception(..))
import Enterprise.USCents (USCents(..), zeroCents)
import Enterprise.USCents as USCents
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.TestM (takeHead)

test :: Spec Unit
test =
  describe "Enterprise.E02IoC" do
    it "When starting with an empty test state then fails unable to find account" do
      expectFailure emptyState $ SystemErrorMessage { internalErrorMessage: "Unable to find BankAccountId (UUID 00000000-0000-0000-0000-000000000000)" }

    expectSuccess
      { given:
          { consoleInput: [ command.quit ]
          , andDateTimes: []
          }
      , then:
          { consoleOutputContains: []
          , andBankAccountNameIs: originalBankAccountName
          , andBankAccountBalanceIs: zeroCents
          }
      }

    expectSuccess
      { given:
          { consoleInput: [ command.checkBalance, command.quit ]
          , andDateTimes: []
          }
      , then:
          { consoleOutputContains: [ "Current Balance: $0.00" ]
          , andBankAccountNameIs: originalBankAccountName
          , andBankAccountBalanceIs: zeroCents
          }
      }

    expectSuccess
      { given:
          { consoleInput: [ command.deposit, "100", command.quit ]
          , andDateTimes: [ "09/21/2025" ]
          }
      , then:
          { consoleOutputContains: [ "The new balance is $100.00" ]
          , andBankAccountNameIs: originalBankAccountName
          , andBankAccountBalanceIs: USCents 100_00
          }
      }

    expectSuccess
      { given:
          { consoleInput:
              [ command.deposit
              , "53"
              , command.deposit
              , "125"
              , command.quit
              ]
          , andDateTimes: [ "09/21/2025", "09/23/2025" ]
          }
      , then:
          { consoleOutputContains:
              [ "The new balance is $53.00"
              , "The new balance is $178.00"
              ]
          , andBankAccountNameIs: originalBankAccountName
          , andBankAccountBalanceIs: USCents 178_00
          }

      }

    expectSuccess
      { given:
          { consoleInput:
              [ command.deposit
              , "53"
              , command.withdraw
              , "30"
              , command.quit
              ]
          , andDateTimes: [ "09/21/2025", "09/23/2025" ]
          }
      , then:
          { consoleOutputContains:
              [ "The new balance is $53.00"
              , "The new balance is $23.00"
              ]
          , andBankAccountNameIs: originalBankAccountName
          , andBankAccountBalanceIs: USCents 23_00
          }
      }

    expectSuccess
      { given:
          { consoleInput:
              [ command.deposit
              , "53"
              , command.withdraw
              , "53"
              , command.quit
              ]
          , andDateTimes: [ "09/21/2025", "09/23/2025" ]
          }
      , then:
          { consoleOutputContains:
              [ "The new balance is $53.00"
              , "The new balance is $0.00"
              ]
          , andBankAccountNameIs: originalBankAccountName
          , andBankAccountBalanceIs: zeroCents
          }
      }

    expectSuccess
      { given:
          { consoleInput:
              [ command.deposit
              , "53"
              , command.withdraw
              , "54"
              , command.quit
              ]
          , andDateTimes: [ "09/21/2025", "09/23/2025" ]
          }
      , then:
          { consoleOutputContains:
              [ "The new balance is $53.00"
              , "Insufficient funds"
              ]
          , andBankAccountNameIs: originalBankAccountName
          , andBankAccountBalanceIs: USCents 53_00
          }
      }
  where
  command =
    { checkBalance: "1"
    , withdraw: "2"
    , deposit: "3"
    , quit: "5"
    }

  reverseMapCommand = case _ of
    "1" -> "<<Check Balance>>"
    "2" -> "<<Withdraw>>"
    "3" -> "<<Deposit>>"
    "4" -> "<<Change Name>>"
    "5" -> "<<Quit>>"
    s -> s

  expectFailure state error = (runTestM state (E02IoC.manageBankAccount bankAccountId)).result `shouldEqual` (Left error)

  expectSuccess :: _ -> Spec Unit
  expectSuccess { given: { consoleInput, andDateTimes }, then: { consoleOutputContains, andBankAccountNameIs, andBankAccountBalanceIs } } =
    describe ("Given input " <> escaped (reverseMapCommand <$> consoleInput)) do
      it "Then it should run without throwing an exceptin" do
        { result } <- run
        result `shouldEqual` (Right unit)

      when (not Array.null consoleOutputContains) $ it ("Then output should contain " <> escaped consoleOutputContains) do
        { state } <- run
        for_ consoleOutputContains \expected ->
          when (not $ Array.any (eq expected) state.consoleOutput) $
            fail ("Expecting '" <> expected <> "' in output: " <> show state.consoleOutput)

      it ("Then the bank balance is " <> USCents.format andBankAccountBalanceIs <> " and the name is '" <> andBankAccountNameIs <> "'") do
        { state } <- run
        bankAccount <- assertJust (error "Unable to find bank account") $ Map.lookup bankAccountId state.bankAccounts
        BankAccount.name bankAccount `shouldEqual` andBankAccountNameIs
        BankAccount.balance bankAccount `shouldEqual` andBankAccountBalanceIs
    where
    escaped strs = String.joinWith ", " $ (\s -> "'" <> s <> "'") <$> strs

    run = do
      dateTimes <-
        -- NOTE: The line below is equivalent to the expanded form. It is what a seasoned Haskeller might write.
        -- `for andDateTimes $ ((JSDate.parse >>> liftEffect) >=> (JSDate.toDateTime >>> maybe (liftEffect $ throw "Failed to convert data") pure))`
        -- Now consider it without module qualification.
        -- `for andDateTimes $ ((parse >>> liftEffect) >=> (toDateTime >>> maybe (liftEffect $ throw "Failed to convert data") pure))`
        -- It is basically REGEX.
        for andDateTimes \dateTimeStr -> do
          jsDate <- liftEffect $ JSDate.parse dateTimeStr
          case JSDate.toDateTime jsDate of
            Nothing -> liftEffect $ throw "Failed to convert data"
            Just date -> pure date

      pure $ runTestM
        ( emptyState
            { bankAccounts = bankAccountPresent
            , consoleInput = consoleInput
            , dateTimes = dateTimes
            }
        )
        (E02IoC.manageBankAccount bankAccountId)

  originalBankAccountName = "Test Account"
  initialBankAccount = BankAccount.mkBankAccount bankAccountId { name: NonEmptyString originalBankAccountName }
  bankAccountPresent = Map.singleton bankAccountId initialBankAccount
  bankAccountId = BankAccountId emptyUUID

--------------------------------------
-- TestM
--------------------------------------

maximumInsurableBalance :: USCents
maximumInsurableBalance = USCents 50_000_00

runTestM :: forall a. TestState -> TestM a -> { result :: Either Exception a, state :: TestState }
runTestM initialState (TestM exceptT) = { result, state }
  where
  Tuple result state = runState (runExceptT exceptT) initialState

-- | TestM is the Test monad that provides the concrete realization of the ports for the purposes of unit testing
newtype TestM a = TestM (ExceptT Exception (State TestState) a)

derive newtype instance Functor TestM
derive newtype instance Apply TestM
derive newtype instance Applicative TestM
derive newtype instance Bind TestM
derive newtype instance Monad TestM
derive newtype instance MonadState TestState TestM
derive newtype instance MonadThrow Exception TestM
derive newtype instance MonadError Exception TestM

type TestState =
  { dateTimes :: Array DateTime
  , bankAccounts :: Map BankAccountId BankAccount
  , bankConfig :: BankConfig
  , consoleOutput :: Array String
  , consoleInput :: Array String
  }

_dateTimes :: Lens' TestState (Array DateTime)
_dateTimes = Lens.lens _.dateTimes _ { dateTimes = _ }

_consoleInput :: Lens' TestState (Array String)
_consoleInput = Lens.lens _.consoleInput _ { consoleInput = _ }

emptyState :: TestState
emptyState =
  { dateTimes: []
  , bankAccounts: Map.empty
  , bankConfig: { maximumInsurableBalance }
  , consoleInput: []
  , consoleOutput: []
  }

instance MonadAsk (HasBankConfig ()) TestM where
  ask = get <#> \{ bankConfig } -> { bankConfig }

systemError :: String -> Exception
systemError internalErrorMessage = SystemErrorMessage { internalErrorMessage }

instance CurrentTime TestM where
  currentTime = takeHead (systemError "Ran out of dateTimes") _dateTimes

instance BankAccountRepository TestM where
  loadBankAccount bankAccountId = do
    { bankAccounts } <- get
    assertJust (systemError $ "Unable to find BankAccountId " <> show bankAccountId) $ Map.lookup bankAccountId bankAccounts
  saveBankAccount bankAccount = do
    modify_ (\s -> s { bankAccounts = Map.insert (BankAccount.id bankAccount) bankAccount s.bankAccounts })
    pure bankAccount

instance Console TestM where
  writeLn str = modify_ \s -> s { consoleOutput = Array.snoc s.consoleOutput str }
  readLn = takeHead (systemError "Ran out of input lines") _consoleInput

module Enterprise.E02Encapsulation
  ( BankAccountId(..)
  , BankAccount -- OBSERVE that the constructor is not exposed. This is what is called an Opaque Data type.
  , _name
  , deposit
  , id
  , mkBankAccount
  , mkBankAccount'
  , balance
  , transactions
  , withdraw
  ) where

-- | Most people assume object oriented concepts such as encapsulation and SOLID no longer apply in functional programming.
-- |
-- | If the OO concept is related to mutation then this is true, but most (almost all) OO best practices have nothing to do with mutation.
-- |
-- | People probably assume OO patterns are no longer necessary because the nature of FP reduces the probability of errors
-- | prevalent in OO imperative languages and this reduces the need to apply design patterns for programs under a certain size. However, those
-- | tools are still available, relevant, and valuable. They just aren't needed unless a software is going to grow beyond a certain point and
-- | that creates a fairly large window for hobby projects, small to medium websites / applications, utilities, and such to skip the pattern. For example
-- | - If pressed for time one might skip OO best practices for programs under 5K LoC but apply them for larger programs. One could "get away with"
-- |   not using SOLID for programs up to 30K LoC. While the software would be less maintainable and it would be slower to add features, a 26K line OO imperative
-- |   program could still make progress without using SOLID.
-- | - Similarly, one might skip applying these patterns in a functional codebase for programs under 15K LoC. One might "get away with" skipping the patterns
-- |   for programs under 45K. Again, this would not be ideal, but if the development team was not comfortable with the practices they could still manage to make
-- |   progress.
-- |
-- | This module provides a simple example of Encapsulation which is typically implemented using Opaque Data Types.
-- |
-- | This module implements a `BankAccount` type that might be represented in C# as the following
-- |
-- | ```csharp
-- | record BankAccountId(Guid Value);
-- |
-- | class InsufficientFunds : Exception { }
-- |
-- | class BankAccount
-- | {
-- |   public BankAccountId Id { get; }
-- |   public List<Transaction> Transactions { get; }
-- |   public string Name { get; set; }
-- |   public USCents Total
-- |   {
-- |     get => new(Transactions.Sum(transaction => transaction.NetEffect.Value));
-- |   }
-- |
-- |   public BankAccount(BankAccountId id, String name)
-- |   {
-- |     Id = id;
-- |     Name = name;
-- |     Transactions = [];
-- |   }
-- |
-- |   public void Deposit(USCents depositAmount)
-- |   {
-- |     Transactions.Add(new(TransactionType.Deposit, depositAmount));
-- |   }
-- |
-- |   public void Withdraw(USCents withdrawAmount)
-- |   {
-- |     if (Total.Value < withdrawAmount.Value) throw new InsufficientFunds();
-- |     Transactions.Add(new(TransactionType.Withdrawal, withdrawAmount));
-- |   }
-- | }
-- | ```
-- |
-- | Here is the rough translation of language patterns
-- | - Constructors: The convention in Haskell family languages is that constructors are named `mkDATATYPE`.
-- | - Read-only properties can be represented as functions `PROPERTYNAME :: DATATYPE -> PROPERTY_DATATYPE`
-- | - Read/Write properties can be represented as a Lens and the convention is to prefix optics with an underscore `_PROPERTYNAME :: Lens' DATATYPE PROPERTY_DATATYPE`
-- | - Operations that update the data type and which CANNOT fail are written `OPERATION :: PARAM1 -> PARAMN -> DATATYPE -> DATATYPE`. That is a function that takes the data type as the
-- |   last argument and which returns a new copy of the DATATYPE.
-- | - Operations that update the data type and which CAN fail are written two different ways. We will cover the more basic in this module
-- |   `OPERATION :: PARAM1 -> PARAM2 -> DATATYPE -> EITHER ERROR_TYPE DATATYPE`. That is a function that takes the data type as the last argument and
-- |   returns an EITHER indicating it could have failed.
-- |
-- | Opaque data types, by encapsulating logic give us the ability to make assumptions about "correct upon construction" in different Apis.

import Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..), note)
import Data.Foldable (foldMap)
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.UUID (UUID)
import Enterprise.BankConfig (BankConfig)
import Enterprise.Transaction (Transaction(..))
import Enterprise.Transaction as Transaction
import Enterprise.TransactionError as TransactionError
import Enterprise.USCents (USCents, ($+))

newtype BankAccountId = BankAccountId UUID

newtype BankAccount = BankAccount BankAccountRecord
type BankAccountRecord =
  { id :: BankAccountId
  , name :: NonEmptyString
  , transactions :: Array Transaction
  }

mkBankAccount :: BankAccountId -> { name :: NonEmptyString } -> BankAccount
mkBankAccount id' { name } =
  BankAccount { id: id', name, transactions: [] }

-- This version of the constructor is what is called a "smart constructor". It can fail. However,
-- because the constructor itself is not exposed engineers know that if they have a `BankAccount` that
-- it is valid.
mkBankAccount' :: BankAccountId -> { name :: String } -> Either String BankAccount
mkBankAccount' id' r = NonEmptyString.fromString r.name
  # note "Name must not be empty"
  <#> \name -> BankAccount { id: id', name, transactions: [] }

id :: BankAccount -> BankAccountId
id = _.id <<< bankAccountRecord

transactions :: BankAccount -> Array Transaction
transactions = _.transactions <<< bankAccountRecord

_name :: Lens' BankAccount NonEmptyString
_name = _bankAccountRecord <<< Lens.lens _.name _ { name = _ }

balance :: BankAccount -> USCents
balance = transactions >>> foldMap Transaction.netEffectOnBalance

-- OBSERVE that this function is fully transparent about the fact that it will throw an error.
-- One might worry that returning an `Either` will make consumption tedious because it seems like
-- engineers would have to immediately handle the error. Anyone who wrote old Windows C code would
-- think back to bloated code that checks results.
-- Fortunately, in Haskell and PureScript `do` notation removes 90% of the tedious boilerplate. More on that later.
--
-- You will also observe the function requires the caller to pass in the current time. This makes the function highly unit testable but can
-- be tedious. We will see how to deal with this later.
withdraw :: { currentTime :: DateTime } -> USCents -> BankAccount -> Either TransactionError.TransactionError BankAccount
withdraw { currentTime } amount bankAccount =
  if balance bankAccount < amount then Left TransactionError.InsufficientFunds
  else Right $ Lens.over _transactions (Array.cons $ Withdrawal { when: currentTime, amount }) bankAccount

deposit :: BankConfig -> { currentTime :: DateTime } -> USCents -> BankAccount -> Either TransactionError.TransactionError BankAccount
deposit config { currentTime } amount bankAccount =
  if balance bankAccount $+ amount > config.maximumInsurableBalance then Left TransactionError.UnableToInsureBalance
  else Right $ Lens.over _transactions (Array.cons $ Deposit { when: currentTime, amount }) bankAccount

---------------------------------------------------------------------------------
-- Private
---------------------------------------------------------------------------------
bankAccountRecord :: BankAccount -> BankAccountRecord
bankAccountRecord (BankAccount r) = r

_bankAccountRecord :: Lens' BankAccount BankAccountRecord
_bankAccountRecord = Lens.lens bankAccountRecord \_ -> BankAccount

_transactions :: Lens' BankAccount (Array Transaction)
_transactions = _bankAccountRecord <<< Lens.lens _.transactions _ { transactions = _ }

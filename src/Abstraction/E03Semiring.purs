module Abstraction.E03Semiring where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala, un)

-- So by this point it may not surprise you that even basic math operators like +, -, *, /, are abstract and can work on lots of different things...

intMath :: Int -> Int
intMath x = (x + 10) * 30

intZero :: Int
intZero = zero

numberMath :: Number -> Number
numberMath x = (x + 10.0) * 30.0

maybeMath :: Maybe Int -> Maybe Int
maybeMath x = (x + (Just 10)) * (Just 30)

maybeZero :: Maybe Int
maybeZero = zero

funcMath1 :: forall a. (a -> Int) -> (a -> Int)
funcMath1 x = (x + (\_ -> 10)) * (\_ -> 30) -- yep, we can add and multiply functions if those functions return semirings...

funcZero :: forall a. a -> Int
funcZero = zero

type SomeRecord = { x :: Int, y :: Number, z :: Maybe Int, q :: String -> Int }

recordMath :: SomeRecord -> SomeRecord
recordMath x = (x + (multInt 10)) * (multInt 30) -- yep, we can add and multiply records if all of the record's fields are semirings

recordZero :: SomeRecord
recordZero = zero

recordOne :: SomeRecord
recordOne = one

-- | Takes ANY Semiring and combines it with itself a fixed integer number of times. This would go in a library.
multInt :: forall a. Semiring a => Int -> a
multInt n =
  -- this is a super advanced bit of combining multiple abstractions. It is using `power` from Monoid, `one` from Semigrind and then
  -- it is wrapping the `one` in additive to specify HOW the value should be combined. `power` can be used to combine any combine-able thing
  -- multiple times. Now here is a reasonable but unfortunate bummer about Semigroup and numbers... while String and Array have just **one** way of combining
  -- and just one identity / empty element a number can be a Monoid in TWO ways: Addition with zero as empty, or multiplication with one as empty. So we have to
  -- first wrap with `Additive` to tell the type system which strategy to use when combining, then call power to combine it a certain number of times
  -- and then we want to unwrap the Additive. Since it is really common to wrap some data, run a function, and then unwrap it... there is
  -- a helper for that called `ala` which works for type wrappers (called Newtype); as in the French "a la" which is used in English as "in the manner of"
  -- or "according to". So the code below is saying, "Hey, take this value and combine it a bunch of times in the manner of additive"
  --
  -- This is beautiful and abstract and powerful... it is also part of the problem with FP languages: multiple abstractions and type machinery
  -- combining.
  ala Additive (flip power n) one

multIntLongHand :: forall a. Semiring a => Int -> a
multIntLongHand n = result
  where
  (Additive result) = power (Additive one) n

multIntLongHand2 :: forall a. Semiring a => Int -> a
multIntLongHand2 = power (Additive one) >>> un Additive

multIntLongHand3 :: forall a. Semiring a => Int -> a
multIntLongHand3 n = power (Additive one) n # un Additive

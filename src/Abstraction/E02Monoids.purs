module Abstraction.E02Monoids where

import Prelude

import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String as String

-- Any data type that can be combined while obeying associativity laws is an instance of a Semigroup.
-- The combination operator is `<>`. One might expect `++` but, presumably, the authors of Haskell/PureScript
-- wanted to differentiate math from the abstract concept of combination.

-- Strings can be combined. Lists, Arrays, Sets, and Maps can be combined.

greetings :: String -> String
greetings name = "Hello " <> name <> "!!!"

numbers :: Array Int -> Array Int
numbers x = [ 1, 2, 3 ] <> x <> [ 100, 101 ]

-- Just like with the Heyting Algebra, a record where all of the fields are instances of Semigroup, is, itself, a Semigroup!

type InnerSemigroupRecord =
  { someText :: String
  , someNumbers :: Array Int
  }

type Weird =
  { text :: String
  , numbers :: Array Int
  , inner :: InnerSemigroupRecord
  }

combineTheWeird :: Weird -> Weird -> Weird
combineTheWeird x y =
  x
    <>
      { text: "--"
      , numbers: [ 0 ]
      , inner: { someText: "!", someNumbers: [ 100 ] }
      }
    <> y

-- And again, just like as with Heyting Algebras, any function that returns a Semigroup is itself a semigroup... so you can combine functions.

type WeirdFn = Int -> Weird

combineWeirdFns :: WeirdFn -> WeirdFn -> WeirdFn
combineWeirdFns x y = x <> y

-- A Monoid is any type that is a Semigroup and ALSO provides an `mempty` (short for Monoid empty) that works like identity with the semigroup combination.

-- The `mempty` for String is the empty string. It is identity because if you combine an empty with any other string it is always equal to exactly that other string.
-- The `mempty` for Array is the empty array.

-- The compiler can derive the mempty value for records whose fields consist entirely of Monoids.

emptyWeird :: Weird
emptyWeird = mempty

-- The compiler can derive the `mempty` for any function whose return is a Monoid.

memptyWeirdFn :: WeirdFn
memptyWeirdFn = mempty

-- Just like with HeytingAlgebras you can make your own data types instances of Monoid.

newtype Increment = Increment Int

instance Semigroup Increment where
  append (Increment x) (Increment y) = Increment $ x + y

instance Monoid Increment where
  mempty = Increment 0

add1 :: Increment -> Increment
add1 = append $ Increment 1

-- There are a rich set of functions that work on Monoids. The first is the convenience function `guard`.

inc10If :: Boolean -> Increment
inc10If b =
  -- `guard b $ Increment 10` is similar to writing
  -- `if b then Increment 10 else Increment 0`
  -- NOTE: In truth the if/then/else expression is more optimal because it only evaluates the true OR the false branch.
  -- The guard function always evaluates the true branch. Developers tend to use guard to clean up / abbreviate their code
  -- but they are mindful to avoid it when it is expensive to compute the true branch.
  guard b $ Increment 10

-- Both PureScript and Haskell offer a `Foldable` abstraction for any container-like data type of 0 to N items. It serves a similar
-- role as `IEnumerable` in .NET. The primary offerings of `Foldable` are `foldl`, `foldr`, and `foldMap` where all of them
-- entail iterating over a structure to build up some result.

-- PureScript does not offer native string interpolation capabilities (in the interest of keeping the core language very small) so
-- developers sometimes need to concatenate a lot of strings long-hand using the `<>` append (combination) operator.

type MiniPerson r =
  { favoriteFood :: String
  , firstName :: String
  , favoriteNumber :: Int
  | r
  }

greetingUsingAppend :: forall r. MiniPerson r -> String
greetingUsingAppend { firstName, favoriteFood, favoriteNumber } =
  "Good morning "
    <> String.toUpper firstName
    <> "!!! I hope you are having a wonderful day. If you are not then consider having "
    <> favoriteFood
    <> " for breakfast. Or you might think about your favorite number "
    <> show favoriteNumber
    <> " which is "
    <> (if favoriteNumber `mod` 2 == 0 then "even" else "odd")
    <> "."

-- In the example above which uses `<>`, you will notice that the if/then/else expression had to be wrapped in parentheses. Additionally, writing
-- `<>` can get a little bit tedious because it is two characters requiring the SHIFT key on the keyboard to access. For long
-- or complex string concatenation expressions developers tend to favor `fold` which takes any Foldable-container of Monoid values
-- and returns a monoid value. Observe below that the if/then/else does not require parentheses in this formulation.

greetingUsingFold :: forall r. MiniPerson r -> String
greetingUsingFold { firstName, favoriteFood, favoriteNumber } =
  fold
    [ "Good morning "
    , String.toUpper firstName
    , "!!! I hope you are having a wonderful day. If you are not then consider having "
    , favoriteFood
    , " for breakfast. Or you might think about your favorite number "
    , show favoriteNumber
    , " which is "
    , if favoriteNumber `mod` 2 == 0 then "even" else "odd"
    , "."
    ]

-- PureScript, Haskell, and Elm lack a null value so null reference exceptions cannot happen in those languages.
-- Instead, missing values are modeled with `Maybe`. (F# and OCaml have the `Option` data type which is essentially the same thing).
-- A Maybe is a container that can have either 0 or 1 items... so it is also `Foldable`. Therefore, it is possible to use
-- `foldMap` with `Maybe` values to apply a function if there is a value or use the monoid empty if a value is missing.

greetingMaybe :: forall r. Maybe (MiniPerson r) -> String
greetingMaybe = foldMap greetingUsingFold

greetingMaybeCase :: forall r. Maybe (MiniPerson r) -> String
greetingMaybeCase maybePerson =
  -- this demonstrates using a case matching to accomplish the same thing
  case maybePerson of
    Nothing -> ""
    Just person -> greetingUsingFold person

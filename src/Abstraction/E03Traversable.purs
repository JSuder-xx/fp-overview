module Abstraction.E03Traversable where

import Prelude

import Data.Either (Either)
import Data.Int as Int
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Traversable (class Foldable, class Traversable, sequence, traverse)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Parsing (Parser)

-- In TypeScript the signature
-- - Promise.all is `all<T>(values: Iterable<T | PromiseLike<T>>): Promise<Awaited<T>[]>`
-- - RxJS has `combineLatest` that will take an array of Observable and return an Observable of the array (https://rxjs.dev/api/index/function/combineLatest).
--
-- But you might also want to convert `Nullable<T>[]` to `Nullable<T[]>`.
-- You might want to convert `Parser<T>[]` to `Parser<T[]>`
--
-- The reality is that all of these operations are fundamentally the exact same thing! However, in languages that lack higher kinded abstractions
-- each library RE-IMPLEMENTS the same functionality and NAMES the functions differently. Worse, in those communities the Api authors are often
-- unaware of the fundamental abstractions so they implement "what makes sense" based on what is needed in a top-down fashion which, when not guided by
-- awarenss of fundamental abstraction, more often than not leads to
-- - missed abstractions
-- - proliferation of low or less ROI functions.
-- - as already mentioned, bespoke names between different libraries. Honestly, it took me 10 minutes to re-locate the correct function in RxJS having using
--   RxJS for a duration of 7 years (with a 4 year gap).
--
-- The examples below show various concrete type signatures with different container types (Array, List, Map, Tuple) and different low level primitives (Aff, Maybe, Either, Parser).
-- Observe that the body of all of these is just `sequence`.

affs :: Array (Aff Int) -> Aff (Array Int)
affs = sequence

affsList :: List (Aff Int) -> Aff (List Int)
affsList = sequence

affMap :: Map String (Aff Int) -> Aff (Map String Int)
affMap = sequence

maybes :: Array (Maybe Int) -> Maybe (Array Int)
maybes = sequence

-- NOTE: A tuple is a container that always has values so it is traversable and you can sequence it.
maybeTuple :: Tuple Int (Maybe String) -> Maybe (Tuple Int String)
maybeTuple = sequence

eithers :: Array (Either String Int) -> Either String (Array Int)
eithers = sequence

parsers :: Array (Parser String Int) -> Parser String (Array Int)
parsers = sequence

-- Just like with most things in PureScript, you can make your own data types `Traversable` and the compiler can even automatically do
-- that for you.

newtype Flurble f = Flurble
  { someData :: f
  , name :: String
  , isCool :: Boolean
  }

derive instance Functor Flurble
derive instance Foldable Flurble
derive instance Traversable Flurble

-- Believe it or not this actually comes up in business cases where there is a single field in a record in an optional or indeterminate state
-- at one point in a pipeline and you want to extract that uncertainty to the edges. Imagine that you have a bunch of functions that work on `Flurble`s
-- where `someData` can be a `Maybe` but then you have an entire set of other functions that work with `Flurble`s where that data is known.
maybeFlurble :: Flurble (Maybe Int) -> Maybe (Flurble Int)
maybeFlurble = sequence

maybeFlurbleLongHand :: Flurble (Maybe Int) -> Maybe (Flurble Int)
maybeFlurbleLongHand (Flurble r@{ name, isCool }) =
  -- without the traverse abstraction we can implement the code long hand... but this is not only
  -- slightly tediuous but could hide the fundamental action.
  r.someData
    # map \someData -> Flurble { someData, name, isCool }

-- Another case is where the `Flurble` has someData in a completely raw state as a string but you want a `Flurble` with that data as an `Int`.
-- Well we can use the `traverse` function, which is the more general (and often the more useful), cousin of `sequence`.  In fact, the two
-- can be implemented in terms of each other
-- ```purescript
-- traverse f traversable = sequence (f <$> traversable)
-- sequence = traverse identity
-- ```
parseFlurble :: Flurble String -> Maybe (Flurble Int)
parseFlurble = traverse Int.fromString

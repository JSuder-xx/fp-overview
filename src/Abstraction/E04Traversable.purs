module Abstraction.E04Traversable where

import Prelude

import Data.Either (Either)
import Data.Int as Int
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Traversable (class Foldable, class Traversable, sequence, traverse)
import Data.Tuple (Tuple)
import Effect.Aff (Aff, ParAff)
import Parsing (Parser)

-- In TypeScript the signature
-- - Promise.all is `all<T>(values: Iterable<T | PromiseLike<T>>): Promise<Awaited<T>[]>`
-- - RxJS has `combineLatest` that will take an array of Observable and return an Observable of the array (https://rxjs.dev/api/index/function/combineLatest).
--
-- But you might also want to convert `Nullable<T>[]` to `Nullable<T[]>`.
-- You might want to convert `Parser<T>[]` to `Parser<T[]>`
--
-- The pattern is `SOMETYPE<TItem>[]` to `SOMETYPE<TItem[]>` or an array of SOMETYPE of TItem to a SOMETYPE of array of TItem.  The pattern is not just
-- a coincidence... it is thing! However, notice in the above, SOMETYPE would have to be a type variable that takes another type variable. Type variables that
-- can take other type variables are called higher kinded. The only production languages I know that can represent higher kinded-ness are PureScript, Haskell, and Scala.
-- In languages that lack higher kinded abstractions each library RE-IMPLEMENTS the same functionality and NAMES the functions differently. Worse, in those communities the Api authors are often
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
affs = sequence -- NOTE: This version runs each aff one after the other sequentially because that is the monadic behavior of `Aff`.

parAffs :: Array (ParAff Int) -> ParAff (Array Int)
parAffs = sequence -- NOTE: This version runs the affs in parallel. The name sequence is referring to how the container (`Array`) is processed and not the things in the array.

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

----------------------------------------------------------------
-- # Case Study RxJS
--
-- Back to RxJS, how might the Api have been better if implemented in a language like PureScript?
--
-- First, if you look at the [Api surface](https://rxjs.dev/api/index) you will find that about 10% of the operators have been
-- deprecated because the authors found a "better" and, presumably, cleaner way to implement the same functionality. This
-- is largely because they were **inventing** Api functions that were actually part of fundamental categories (Functors, Monads)
-- which should have been **discovered**. When building up a low-level Api with categories one is very unlikely to get it wrong
-- so long as the implementation of the categories obeys the prescribed laws.
--
-- Second, about 70 - 80% of the operators belong to said categories so most of the Api surface wouldn't even need to be documented because
-- it would be familiar to engineers already familiar with those categories!!! Additionally, engineers experienced with the categories would
-- immediately not only know what functions are available but have all of the tips, tricks, and expert usage patterns already in their minds
-- and ready to go. They would be instant masters of most of the Api with zero training.
--
-- The truth is that an Observable is
-- - Functor (map)
-- - Apply (apply :: forall a b. f (a -> b) -> f a -> f b)
-- - Applicative (pure :: forall a. a -> f a)
-- - Bind/Monad (bind :: forall a b. m a -> (a -> m b) -> m b)
-- - Traversable
-- - [Filterable] (https://pursuit.purescript.org/packages/purescript-filterable/5.0.0/docs/Data.Filterable)
--
-- and this combination of categories provides an ENORMOUS Api surface.
--
-- Additionally, there are multiple versions of each combining operation based on whether to do so in sequence or in parallel and this
-- results in a combinatorial explosion of functions without a clear naming strategy which is cluttered/noisy and error prone.
--
-- In FP codebases, when categorial behavior needs to change then developers create a NEW data type that simply wraps the other data type
-- and overrides the behavior which makes the behavior explicit in the data types! In fact, the specific notion that RxJS contents with of
-- of running sequentially or in parallel and the need to convert back and forth between these two representations is already abstracted
-- in PureScript with the `Parallel` type class.
--
-- With Aff (PureScript's Future - which is a cold version of a Promise) the computations run sequentially. However, if you want
-- your asynchronous stuff to run in parallel you can convert that to a `ParAff`. The `ParAff` has `Functor`, `Applicative`, but NOT `Monad`
-- because the `ParAff` is, as the name implies, designed to be run in parallel.
--
-- So the Observable would probably have an `Observable` and a `ParObservable` types with a `Parallel` instance to convert back and forth
-- using a standardized (familiar) Api. Then developers wouldn't have to learn entirely new functions but would simply convert their observable
-- as necessary and continue to use `map` (`<$>`), `apply` (`<*>`), `bind` (`>>=`), and the dozens of other functions which they already know.
--
-- So long story short
-- - About 70 - 80% of the operators collapse down to things engineers already know.
-- - Behavior would be **explicitly** modeled in types by having an `Observable` and a `ParObservable` rather than implicitly.
--
-- The beauty of this is that the remaining 20 - 30% of the Api which is specific to observables becomes tremendously more clear
-- and more digestible.

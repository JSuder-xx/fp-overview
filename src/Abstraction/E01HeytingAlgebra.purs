module Abstraction.E01HeytingAlgebra where

import Prelude

import Control.Apply (lift2)
import Data.Either (Either, note)
import Data.HeytingAlgebra (ff, tt)
import Data.Map (Map)
import Data.Map as Map

-- The Boolean operators for conjunction (&&, and), disjunction (||, or), and negation (not) operate only on Boolean values for most programming languages.

-- The operators work on Boolean values in PureScript as well...

falseAndTrue :: Boolean
falseAndTrue = false && true

falseOrTrue :: Boolean
falseOrTrue = false || true

notFalse :: Boolean
notFalse = not false

-- HOWEVER, in PureScript the operators work on ANY data type that is an instance of a `HeytingAlgebra` which basically means anything that follows the same
-- general laws. The algebra includes `&&`, `||`, and `not` operators along with `ff` for false, `tt` for true, and the `implies` function.

-- In PureScript, a record where every field is a `HeytingAlgebra` is, itself, an instance of a `HeytingAlgebra`! That means you can use the conjunction and disjunction operators on records
-- if their fields are all HeytingAlgebras.

type BunchOfBools =
  { hasWidget :: Boolean
  , hasGadget :: Boolean
  , hasGidget :: Boolean
  }

allTheBunch :: BunchOfBools
allTheBunch = tt

noneOfTheBunch :: BunchOfBools
noneOfTheBunch = ff

anyOfTheBunch :: BunchOfBools -> BunchOfBools -> BunchOfBools
anyOfTheBunch x y = x || y

-- In PureScript, any function that returns a `HeytingAlgebra` is, itself, an instance of a `HeytingAlgebra`!!! This means you can combine predicates!

type Person = { heightInInches :: Int, weightInLbs :: Int }

canRideKiddieRide :: Person -> Boolean
canRideKiddieRide person = person.heightInInches >= 32

canRideWaterSlide :: Person -> Boolean
canRideWaterSlide person = person.weightInLbs < 250

canRideDeathDragon :: { worriedAboutGettingSued :: Boolean } -> Person -> Boolean
canRideDeathDragon { worriedAboutGettingSued } p = (not worriedAboutGettingSued) || (p.heightInInches > 60 && p.weightInLbs < 220)

canRideEverything :: Person -> Boolean
canRideEverything =
  -- OBSERVE using && on three FUNCTIONS
  canRideKiddieRide && canRideWaterSlide && canRideDeathDragon { worriedAboutGettingSued: true }

-- Combining predicates this way is not only more succinct but it can also be safer than writing a lambda long-hand when that lambda is nested deep inside another function
-- because when writing a lambda it is always possible to accidentally capture and use a value in the enclosing scope. For example, in the code below `george` and `cindy` are used in
-- two of the cases rather than `person` in all three.
-- around.
-- ```purescript
-- someFunction allPeople george cindy =
--   -- some body
--   where
--   allCanRide :: Boolean
--   allCanRide = allPeople # Array.all \person -> canRideKiddieRide person && canRideWaterSlide george && canRideDeathDragon { worriedAboutGettingSued: true } cindy
-- ```

-- The ability recurses indefinitely.

type NestedExample =
  { bool :: Boolean
  , pred :: Int -> Person -> BunchOfBools
  }

orExample :: NestedExample -> NestedExample -> NestedExample
orExample x y =
  -- This is a record where one of the fields is a FUNCTION that returns a RECORD of booleans...
  -- and we can OR them.
  x || y

-- Further, you can make your own data types instances of HeytingAlgebras.

-- here is a sum type with two cases.
data Temperature = Hot | Cold

derive instance Eq Temperature

instance HeytingAlgebra Temperature where
  ff = Cold
  tt = Hot
  not x = if (x == Hot) then Cold else Hot
  conj x y = if (x == Hot) && (y == Hot) then Hot else Cold
  disj x y = if (x == Cold) && (y == Cold) then Cold else Hot
  implies x y = not x || y

-- This can be useful if building a DSL. For example, here is a tiny for Boolean expression DSL. More useful
-- real-world cases might be SQL builders or other DSLs over grammars with boolean logic.

data BoolExpr
  = Literal Boolean
  | ReadVariable String
  | Or BoolExpr BoolExpr
  | And BoolExpr BoolExpr
  | Not BoolExpr

instance HeytingAlgebra BoolExpr where
  ff = Literal false
  tt = Literal true
  not = Not
  conj = And
  disj = Or
  implies x y = not x || y

-- a little interpreter for our DSL
runBoolExpr :: BoolExpr -> Map String Boolean -> Either String Boolean
runBoolExpr rootExpr variables = interpret' rootExpr
  where
  interpret' = case _ of
    Literal x -> pure x
    ReadVariable name -> note ("Unable to find variable '" <> name <> "'") $ Map.lookup name variables
    Or x y -> lift2 (||) (interpret' x) (interpret' y)
    And x y -> lift2 (&&) (interpret' x) (interpret' y)
    Not x -> not <$> interpret' x

exampleExpression :: BoolExpr
exampleExpression =
  -- observe that we can use `not`, `||`, and `&&` to build up an expression in our small DSL
  not (ReadVariable "x" || ReadVariable "y") && ReadVariable "y"

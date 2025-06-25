module Enterprise.USCents where

import Prelude

import Data.Either (Either, note)
import Data.Int as Int
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)

-- we would model this with a Money or currency type but keeping it simple for this exercise
newtype USCents = USCents Int

derive instance Eq USCents
derive instance Ord USCents
derive newtype instance Show USCents

instance Semigroup USCents where
  append = addCents

instance Monoid USCents where
  mempty = zeroCents

-- Observe that we can define custom infix operators for adding and subtracting USCents.
infixl 6 addCents as $+
infixl 6 subtractCents as $-

zeroCents :: USCents
zeroCents = USCents 0

addCents :: USCents -> USCents -> USCents
addCents (USCents x) (USCents y) = USCents $ x + y

subtractCents :: USCents -> USCents -> USCents
subtractCents (USCents x) (USCents y) = USCents $ x - y

format :: USCents -> String
format (USCents cents) = "$" <> (toStringWith (fixed 2) $ (Int.toNumber cents) / 100.0)

parse :: String -> Either String USCents
parse = note "Invalid value" <<< map (USCents <<< Int.floor <<< mul 100.0) <<< Number.fromString

module Enterprise.E02IoC.Ports.Console
  ( class Console
  , displayOptions
  , readLn
  , writeLn
  , readLnParsed
  , prompt
  , promptYesNo
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String

-- | An ABSTRACT representation of a monad that implements console-like behavior or a text user interface.
-- | This abstraction can be implemented for unit tests using a concrete State data type where
-- | - WriteLn operations are written to an array.
-- | - ReadLn operations pop a value off an array.
-- |
-- | The unit tests initialize the state with an array of strings that should act as inputs
-- | and upon completion the tests can inspect 1. Values written to a log. 2. How many of the inputs remain.
class Monad m <= Console m where
  writeLn :: String -> m Unit
  readLn :: m String

readLnParsed :: forall m a. Console m => (String -> Either String a) -> m (Maybe a)
readLnParsed parse = do
  rawString <- readLn
  case parse rawString of
    Left errorMessage -> do
      writeLn errorMessage
      tryAgain <- promptYesNo { default: false } "Try again?"
      if not tryAgain then pure Nothing
      else readLnParsed parse
    Right x -> pure $ Just x

displayOptions :: forall m r. Console m => Array { char :: Char, description :: String | r } -> m Unit
displayOptions = traverse_ \option -> writeLn $ charToString option.char <> " - " <> option.description

prompt :: forall m a r. Eq a => Console m => { default :: Maybe a } -> Array { char :: Char, value :: a | r } -> String -> m a
prompt { default } options promptMessage = go
  where
  go = do
    writeLn $ promptMessage <> "? " <> defaultPrompt
    response <- readLn
    if response == "" then default # maybe go pure
    else
      options
        # Array.find (_.char >>> charToString >>> eq response)
        # maybe go (pure <<< _.value)

  defaultPrompt = String.joinWith "/" $ case default of
    Nothing -> options <#> _.char >>> charToString
    Just def -> options <#> \option -> if option.value == def then "(" <> charToString option.char <> ")" else charToString option.char

promptYesNo :: forall m. Console m => { default :: Boolean } -> String -> m Boolean
promptYesNo { default } = prompt
  { default: Just default }
  [ { char: 'Y', value: true }, { char: 'N', value: false } ]

charToString :: Char -> String
charToString = String.codePointFromChar >>> Array.singleton >>> String.fromCodePointArray

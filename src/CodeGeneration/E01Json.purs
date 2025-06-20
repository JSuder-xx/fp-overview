module CodeGeneration.E01Json where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, getField, (.!=), (.:), (.:?))
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

------------------------------
-- Encoding/Decoding Sum Types
------------------------------

data Icecream
  = Chocolate
  | Strawberry
  | Vanilla
  | Other String

-- We ask the compiler to generate "Generic" representations for the Icecream type. This basically allows
-- us to work with the type metadata and is related to reflection. This is the magic sauce that empowers library
-- authors to write code generating code.
derive instance Generic Icecream _

-- With a Generic instance you can ask the compiler to automatically generate Json encoding and decoding for the sum type.

instance EncodeJson Icecream where
  encodeJson = genericEncodeJson

instance DecodeJson Icecream where
  decodeJson = genericDecodeJson

----------------------------
-- Encoding/Decoding Records
----------------------------

type Person =
  { firstName :: String
  , lastName :: String
  , isCool :: Boolean
  , favoriteIcecream :: Maybe Icecream
  }

-- if every field in a record has an `EncodeJson` instance then you can use `encodeJson` without writing any manual code.
encodePerson :: Person -> Json
encodePerson = encodeJson

-- if every field in a record has a `DecodeJson` instance then you can use `encodeJson` without writing any manual code.
decodePerson :: Json -> Either JsonDecodeError Person
decodePerson = decodeJson

-----------------------------
-- Encoding/Decoding Explicit
-----------------------------

-- Of course, sometimes

encodePersonCustom :: Person -> Json
encodePersonCustom person =
  "fname" := person.firstName
    ~> "lname" := person.lastName
    ~> "cool" := person.isCool
    ~> "f_icecream" := person.favoriteIcecream
    ~> jsonEmptyObject

decodePersonCustom :: Json -> Either JsonDecodeError Person
decodePersonCustom = decodeJson >=> \obj -> do
  firstName <- obj .: "fname"
  lastName <- obj .: "lname"
  isCool <- obj .: "cool"
  favoriteIcecream <-
    -- by using .:? we say the field can be absent from the Json and then using .!= we set the default value
    obj .:? "f_icecream" .!= Nothing
  pure { firstName, lastName, isCool, favoriteIcecream }

decodePersonCustom2 :: Json -> Either JsonDecodeError Person
decodePersonCustom2 = decodeJson >=> decodeObject
  where
  decodeObject :: Object Json -> Either JsonDecodeError Person
  decodeObject obj = ado
    firstName <- field "fname"
    lastName <- field "lname"
    isCool <- field "cool"
    favoriteIcecream <- field "f_icecream"
    in
      { firstName, lastName, isCool, favoriteIcecream }
    where
    field :: forall a. DecodeJson a => String -> Either JsonDecodeError a
    field = getField obj

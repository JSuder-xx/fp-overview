module Abstraction.E01HeytingAlgebra.BoolExpr
  ( BoolExpr -- CONSTRUCTORS NOT EXPORTED
  , runBoolExpr
  , readVariable
  , exampleExpression
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons)

import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)

data BoolExpr (variablesRow :: Row Type)
  = Literal Boolean
  | ReadVariable String
  | Or (BoolExpr variablesRow) (BoolExpr variablesRow)
  | And (BoolExpr variablesRow) (BoolExpr variablesRow)
  | Not (BoolExpr variablesRow)

instance HeytingAlgebra (BoolExpr r) where
  ff = Literal false
  tt = Literal true
  not = Not
  conj = And
  disj = Or
  implies x y = not x || y

-- a little interpreter for our DSL
runBoolExpr :: forall variablesRow. Homogeneous variablesRow Boolean => BoolExpr variablesRow -> Record variablesRow -> Boolean
runBoolExpr rootExpr variablesRecord = interpret' rootExpr
  where
  variables :: Object Boolean
  variables = Object.fromHomogeneous variablesRecord

  interpret' = case _ of
    Literal x -> x
    ReadVariable name -> fromMaybe false $ Object.lookup name variables
    Or x y -> (interpret' x) || (interpret' y)
    And x y -> (interpret' x) && (interpret' y)
    Not x -> not $ interpret' x

readVariable
  :: forall @fieldName r r'
   . IsSymbol fieldName
  => Cons fieldName Boolean r' r
  => BoolExpr r
readVariable = ReadVariable $ reflectSymbol (Proxy :: Proxy fieldName)

exampleExpression :: BoolExpr (x :: Boolean, y :: Boolean, z :: Boolean)
exampleExpression =
  -- observe that we can use `not`, `||`, and `&&` to build up an expression in our small DSL
  not (readVariable @"x" || readVariable @"y") && readVariable @"z"

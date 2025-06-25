module Test.Abstraction.BoolExpr where

import Prelude

import Abstraction.E01HeytingAlgebra.BoolExpr (BoolExpr, readVariable, runBoolExpr)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

test :: Spec Unit
test =
  describe "Abstraction.BoolExpr" do
    it "When starting with an empty test state then fails unable to find account" do
      runBoolExpr exampleExpression { x: false, y: false, z: false } `shouldEqual` false
      runBoolExpr exampleExpression { x: false, y: false, z: true } `shouldEqual` true
      runBoolExpr exampleExpression { x: false, y: true, z: false } `shouldEqual` false
      runBoolExpr exampleExpression { x: false, y: true, z: true } `shouldEqual` false
  where
  exampleExpression :: BoolExpr (x :: Boolean, y :: Boolean, z :: Boolean)
  exampleExpression =
    -- observe that we can use `not`, `||`, and `&&` to build up an expression in our small DSL
    not (readVariable @"x" || readVariable @"y") && readVariable @"z"

module Test.Main where

import Prelude

import Effect (Effect)
import Test.Abstraction.BoolExpr as BoolExpr
import Test.Enterprise.E02IoC as Enterprise.E02IoC
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  BoolExpr.test
  Enterprise.E02IoC.test

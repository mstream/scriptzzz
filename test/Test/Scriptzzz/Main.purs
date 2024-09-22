module Test.Scriptzzz.Main where

import Prelude

import Data.Map (fromFoldable)
import Data.String.NonEmpty as NES
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Scriptzzz.Game.Command (Commands, UnitCommands(..))
import Scriptzzz.Game.Types (Id(..))
import Scriptzzz.Sandbox (ExecutionResult(..))
import Scriptzzz.Sandbox as Sandbox
import Test.Scriptzzz.App.Controller.Handler.EditorUpdated as EditorUpdated
import Test.Scriptzzz.App.Controller.Handler.ScriptEvaluated as ScriptEvaluated
import Test.Scriptzzz.App.Controller.Handler.TimeUpdated as TimeUpdated
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Utils (detailedErrorConsoleReporter)
import Type.Proxy (Proxy(..))
import Yoga.JSON as JSON

main ∷ Effect Unit
main = runSpecAndExitProcess [ detailedErrorConsoleReporter ] spec

spec ∷ Spec Unit
spec = do
  EditorUpdated.spec
  ScriptEvaluated.spec
  TimeUpdated.spec
  describe "Command" do
    it "codes JSON properly" do
      let
        unitId ∷ Id
        unitId = Id $ NES.nes (Proxy ∷ _ "abc")

        commands ∷ Commands
        commands =
          { workers:
              { moveTo: UnitCommands $ fromFoldable
                  [ unitId /\ { position: { x: 1, y: 2 } } ]
              }
          }

        actual ∷ String
        actual = JSON.writeJSON commands

        expected ∷ String
        expected =
          """{"workers":{"moveTo":{"abc":{"position":{"y":2,"x":1}}}}}"""

      actual `shouldEqual` expected

  describe "Sandbox/execJs" do
    it "returns valid output" do
      executionResult ← Sandbox.runProgram (Milliseconds 1000.0)
        "return 3"
      executionResult `shouldEqual` Success 3


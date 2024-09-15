module Test.Scriptzzz.Main where

import Prelude

import Data.Map (fromFoldable)
import Data.Map as Map
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Scriptzzz.Command (Commands, UnitCommands(..))
import Scriptzzz.Engine (Id(..))
import Scriptzzz.Sandbox as Sandbox
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Type.Proxy (Proxy(..))
import Yoga.JSON as JSON

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  describe "Command" do
    it "codes JSON properly" do
      let
        unitId :: Id
        unitId = Id $ NES.nes (Proxy :: _ "abc")

        commands :: Commands
        commands = { harvest: UnitCommands Map.empty, moveTo: UnitCommands $ fromFoldable [ unitId /\ { position: { x: 1, y: 2 } } ] }

        actual :: String
        actual = JSON.writeJSON commands

        expected :: String
        expected = """{"moveTo":{"abc":{"position":{"y":2,"x":1}}}}"""

      actual `shouldEqual` expected

  describe "Sandbox/execJs" do
    it "returns valid output" do
      outputString <- Sandbox.execJs 1000 "return 3"
      outputString `shouldEqual` "3"


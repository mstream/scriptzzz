module Test.Scriptzzz.Game.Command (spec) where

import Scriptzzz.Prelude

import Data.Map as M
import Scriptzzz.Core (Id, makeId)
import Scriptzzz.Game as Game
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.JSON as JSON

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.Game.Command" do
    describe "Command" do
      it "codes JSON properly" do
        let
          unitId ∷ Id
          unitId = makeId (Proxy ∷ _ "abc")

          commands ∷ Game.Commands
          commands =
            { workers:
                { moveTo: Game.UnitCommands $ M.fromFoldable
                    [ unitId /\ { position: { x: 1, y: 2 } } ]
                }
            }

          actual ∷ String
          actual = JSON.writeJSON commands

          expected ∷ String
          expected =
            """{"workers":{"moveTo":{"abc":{"position":{"y":2,"x":1}}}}}"""

        actual `shouldEqual` expected



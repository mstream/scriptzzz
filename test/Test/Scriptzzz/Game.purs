module Test.Scriptzzz.Game (spec) where

import Scriptzzz.Prelude

import Data.Map as M
import Scriptzzz.Core (Id, makeId)
import Scriptzzz.Game as Game
import Scriptzzz.PathFinding as PF
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.Game" do
    it "evaluates game state properly" do
      let
        environment ∷ Game.Environment
        environment = { obstacleMatrix: PF.emptyObstacleMatrix }

        workerId ∷ Id
        workerId = makeId (Proxy ∷ _ "foo")

        previousState ∷ Game.State
        previousState = Game.State $ M.singleton
          workerId
          (Game.Worker { position: { x: 1, y: 2 }, task: Nothing })

        commands ∷ Game.Commands
        commands =
          { workers:
              { moveTo: Game.UnitCommands $ M.singleton workerId
                  { position: { x: 3, y: 4 } }
              }
          }

        actualState /\ actualLogs = Game.update
          environment
          commands
          previousState

        expectedState ∷ Game.State
        expectedState = Game.State $ M.singleton
          workerId
          ( Game.Worker
              { position: { x: 2, y: 3 }
              , task: Just
                  { path: PF.buildPath { x: 3, y: 4 } []
                  , targetPosition: { x: 3, y: 4 }
                  }
              }
          )

        expectedLogs ∷ Game.Logs
        expectedLogs = []

      actualState `shouldEqual` expectedState
      actualLogs `shouldEqual` expectedLogs


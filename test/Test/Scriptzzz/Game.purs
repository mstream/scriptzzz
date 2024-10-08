module Test.Scriptzzz.Game (spec) where

import Scriptzzz.Prelude

import Data.Map as M
import Data.Typelevel.Num (D4, d0, d1, d2, d3)
import Scriptzzz.Core (Id, makeId, makePosition)
import Scriptzzz.Game as Game
import Scriptzzz.PathFinding as PF
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.Game" do
    it "evaluates game state properly" do
      let
        environment ∷ Game.Environment D4 D4
        environment = { obstacleMatrix: PF.emptyObstacleMatrix }

        workerId ∷ Id
        workerId = makeId (Proxy ∷ _ "foo")

        previousState ∷ Game.State D4 D4
        previousState = Game.State $ M.singleton
          workerId
          (Game.Worker { position: makePosition d0 d1, task: Nothing })

        commands ∷ Game.Commands D4 D4
        commands =
          { workers:
              { moveTo: Game.UnitCommands $ M.singleton workerId
                  { position: makePosition d2 d3 }
              }
          }

        actualState /\ actualLogs = Game.update
          environment
          commands
          previousState

        expectedState ∷ Game.State D4 D4
        expectedState = Game.State $ M.singleton
          workerId
          ( Game.Worker
              { position: makePosition d1 d2 
              , task: Just
                  { path: PF.buildPath (makePosition d2 d3)  []
                  , targetPosition: makePosition d2 d3
                  }
              }
          )

        expectedLogs ∷ Game.Logs
        expectedLogs = []

      actualState `shouldEqual` expectedState
      actualLogs `shouldEqual` expectedLogs


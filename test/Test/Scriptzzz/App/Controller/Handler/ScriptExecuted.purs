module Test.Scriptzzz.App.Controller.Handler.ScriptExecuted (spec) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.Map as M
import Data.String.NonEmpty as NES
import Data.Time.Duration (Milliseconds(..))
import Data.Typelevel.Num (D4, d0, d1, d2, d3)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.ScriptExecuted as CanvasInitialized
import Scriptzzz.App.Message (ScriptExecutedPayload)
import Scriptzzz.App.Model (Model(..), SimulatingModel)
import Scriptzzz.App.Model.AnimationState
  ( AnimationState(..)
  , initialGameStep
  , nextGameStep
  )
import Scriptzzz.Core (Id, makeId, makePosition)
import Scriptzzz.Game as Game
import Scriptzzz.PathFinding as PF
import Scriptzzz.Sandbox as Sandbox
import Test.Flame.Update.Handler as TH
import Test.Flame.Update.Handler.Scriptzzz
  ( ModelAssertionConfig
  , RunFailureScenario
  , RunSuccessScenario
  )
import Test.Spec (Spec, describe, it)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.ScriptExecuted" do
    it "fails when in wrong mode" do
      runFailureScenario 10 do
        t0 ← TH.nextTimeTickTimestamp
        t1 ← TH.nextTimeTickTimestamp
        pure do
          previousModel ∷ Model D4 D4 ← arbitrary `suchThat` case _ of
            Simulating _ →
              false

            _ → true

          let
            messagePayload ∷ ScriptExecutedPayload D4 D4
            messagePayload =
              { executionFinishTime: t1
              , executionResult: Sandbox.ScriptReturnValueNotReceived
                  $ Sandbox.ExecutionTimeout
                  $ Milliseconds 123.0
              , executionStartTime: t0
              }

            expectedErrorMessage ∷ String
            expectedErrorMessage =
              "Not in simulating mode."

          pure
            { expectedErrorMessage
            , messagePayload
            , previousModel
            }

    it "handles script error with 'stop on error' option is enabled" do
      runSuccessScenario 10 do
        t0 ← TH.nextTimeTickTimestamp
        t1 ← TH.nextTimeTickTimestamp
        pure do
          simulatingModel ← arbitrary <#>
            \(model ∷ SimulatingModel D4 D4) →
              model
                { animationState = Updated { gameStep: initialGameStep }
                , editor = model.editor
                    { gameSettings = model.editor.gameSettings
                        { stopOnError = true }
                    }
                }

          executionResult ← arbitrary `suchThat` case _ of
            Sandbox.Success _ →
              false

            _ → true

          let
            previousModel ∷ Model D4 D4
            previousModel = Simulating simulatingModel

            messagePayload ∷ ScriptExecutedPayload D4 D4
            messagePayload =
              { executionFinishTime: t1
              , executionResult
              , executionStartTime: t0
              }

            modelExpectations
              ∷ ∀ m
              . TH.Assert m
              ⇒ MonadAsk (ModelAssertionConfig D4 D4) m
              ⇒ m Unit
            modelExpectations = do
              { nextModel } ← ask
              case nextModel of
                Editing editingModel →
                  TH.assertEqual
                    { actual: editingModel.lastScriptExecution
                    , description: NES.nes
                        ( Proxy
                            ∷ _
                                "script execution result should be set"
                        )
                    , expected: Just
                        { finishTime: t1
                        , result: executionResult
                        , startTime: t0
                        }
                    }

                _ →
                  TH.fail $ NES.nes (Proxy ∷ _ "not in editing mode")

            expectedCommands ∷ Cmd.Commands D4 D4
            expectedCommands = Cmd.none

          pure
            { expectedCommands
            , messagePayload
            , modelExpectations
            , previousModel
            }

    it "updates animation on success" do
      runSuccessScenario 10 do
        t0 ← TH.nextTimeTickTimestamp
        t1 ← TH.nextTimeTickTimestamp
        pure do
          let
            workerId ∷ Id
            workerId = makeId (Proxy ∷ _ "foo")

          simulatingModel ← arbitrary <#>
            \(model ∷ SimulatingModel D4 D4) →
              model
                { animationState = Updated { gameStep: initialGameStep }
                , gameState = Game.State $ M.singleton
                    workerId
                    ( Game.Worker
                        { position: makePosition d0 d1, task: Nothing }
                    )
                }

          let
            executionResult
              ∷ Sandbox.ExecutionResult (Game.Commands D4 D4)
            executionResult =
              Sandbox.Success
                { workers:
                    { moveTo: Game.UnitCommands $ M.singleton
                        workerId
                        { position: makePosition d2 d3 }
                    }
                }

            previousModel ∷ Model D4 D4
            previousModel = Simulating simulatingModel

            messagePayload ∷ ScriptExecutedPayload D4 D4
            messagePayload =
              { executionFinishTime: t1
              , executionResult
              , executionStartTime: t0
              }

            modelExpectations
              ∷ ∀ m
              . TH.Assert m
              ⇒ MonadAsk (ModelAssertionConfig D4 D4) m
              ⇒ m Unit
            modelExpectations = do
              { nextModel } ← ask
              case nextModel of
                Simulating actualSimulatingModel → do
                  TH.assertEqual
                    { actual:
                        actualSimulatingModel.editor.lastScriptExecution
                    , description: NES.nes
                        ( Proxy
                            ∷ _
                                "script execution info should be set"
                        )
                    , expected: Just
                        { finishTime: t1
                        , result: executionResult
                        , startTime: t0
                        }
                    }

                  TH.assertEqual
                    { actual: actualSimulatingModel.gameState
                    , description: NES.nes
                        (Proxy ∷ _ "game state should be updated")
                    , expected: Game.State $ M.singleton
                        workerId
                        ( Game.Worker
                            { position: makePosition d1 d2
                            , task: Just
                                { path: PF.buildPath
                                    (makePosition d2 d3)
                                    []
                                , targetPosition: makePosition d2 d3
                                }
                            }
                        )
                    }

                  TH.assertEqual
                    { actual: actualSimulatingModel.animationState
                    , description: NES.nes
                        ( Proxy
                            ∷ _
                                "animation state should be set to updating"
                        )
                    , expected: Updating
                        { gameStep: nextGameStep initialGameStep }
                    }

                _ →
                  TH.fail $ NES.nes (Proxy ∷ _ "not in simulating mode")

            expectedCommands ∷ Cmd.Commands D4 D4
            expectedCommands = Cmd.none `Cmd.withUpdateAnimation`
              { animation:
                  { createEntity: []
                  , destroyEntity: []
                  , updateEntity:
                      [ { id: workerId
                        , sourcePosition: makePosition d0 d1
                        , targetPosition: makePosition d1 d2
                        }
                      ]
                  }
              , gameStep: nextGameStep initialGameStep
              }

          pure
            { expectedCommands
            , messagePayload
            , modelExpectations
            , previousModel
            }

runFailureScenario
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ RunFailureScenario w h (ScriptExecutedPayload w h)
runFailureScenario = TH.makeRunFailureScenario CanvasInitialized.handle

runSuccessScenario
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ RunSuccessScenario w h (ScriptExecutedPayload w h)
runSuccessScenario = TH.makeRunSuccessScenario CanvasInitialized.handle

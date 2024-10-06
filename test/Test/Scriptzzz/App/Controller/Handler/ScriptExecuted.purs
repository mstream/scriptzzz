module Test.Scriptzzz.App.Controller.Handler.ScriptExecuted (spec) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.Map as M
import Data.String.NonEmpty as NES
import Data.Time.Duration (Milliseconds(..))
import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.ScriptExecuted as CanvasInitialized
import Scriptzzz.App.Message (ScriptExecutedPayload)
import Scriptzzz.App.Model (Model(..), SimulatingModel)
import Scriptzzz.App.Model.AnimationState
  ( AnimationState(..)
  , initialGameStep
  , nextGameStep
  )
import Scriptzzz.Core (Id, makeId)
import Scriptzzz.Game as Game
import Scriptzzz.PathFinding as PF
import Scriptzzz.Sandbox as Sandbox
import Test.Flame.Update.Handler
  ( ConfigM
  , nextTimeTickTimestamp
  , runFailureScenario
  , runSuccessScenario
  )
import Test.Flame.Update.Handler as TH
import Test.Flame.Update.Handler.Scriptzzz
  ( HandlerFailureScenarioConfig
  , HandlerSuccessScenarioConfig
  , ModelAssertionConfig
  )
import Test.Spec (Spec, describe, it)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.ScriptExecuted" do
    it "fails when in a state different than Simulating"
      $ runScriptExecutedFailureScenario 10 do
          t0 ← nextTimeTickTimestamp
          t1 ← nextTimeTickTimestamp
          pure do
            previousModel ← arbitrary `suchThat` case _ of
              Simulating _ →
                false

              _ → true

            let
              messagePayload ∷ ScriptExecutedPayload
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

    it
      "goes back to edit mode when script fails and 'stop on error' option is enabled"
      do
        runScriptExecutedSuccessScenario 10 do
          t0 ← nextTimeTickTimestamp
          t1 ← nextTimeTickTimestamp
          pure do
            simulatingModel ← arbitrary <#> \(model ∷ SimulatingModel) →
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
              previousModel ∷ Model
              previousModel = Simulating simulatingModel

              messagePayload ∷ ScriptExecutedPayload
              messagePayload =
                { executionFinishTime: t1
                , executionResult
                , executionStartTime: t0
                }

              modelExpectations
                ∷ ∀ m
                . TH.Assert m
                ⇒ MonadAsk ModelAssertionConfig m
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

              expectedCommands ∷ { | Commands CommandParameters }
              expectedCommands = Cmd.none

            pure
              { expectedCommands
              , messagePayload
              , modelExpectations
              , previousModel
              }

    it "should update animation on successful execution" do
      runScriptExecutedSuccessScenario 10 do
        t0 ← nextTimeTickTimestamp
        t1 ← nextTimeTickTimestamp
        pure do
          let
            workerId ∷ Id
            workerId = makeId (Proxy ∷ _ "foo")

          simulatingModel ← arbitrary <#> \(model ∷ SimulatingModel) →
            model
              { animationState = Updated { gameStep: initialGameStep }
              , gameState = Game.State $ M.singleton
                  workerId
                  ( Game.Worker
                      { position: { x: 1, y: 2 }, task: Nothing }
                  )
              }

          let
            executionResult ∷ Sandbox.ExecutionResult Game.Commands
            executionResult =
              Sandbox.Success
                { workers:
                    { moveTo: Game.UnitCommands $ M.singleton
                        workerId
                        { position: { x: 3, y: 4 } }
                    }
                }

            previousModel ∷ Model
            previousModel = Simulating simulatingModel

            messagePayload ∷ ScriptExecutedPayload
            messagePayload =
              { executionFinishTime: t1
              , executionResult
              , executionStartTime: t0
              }

            modelExpectations
              ∷ ∀ m
              . TH.Assert m
              ⇒ MonadAsk ModelAssertionConfig m
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
                            { position: { x: 2, y: 3 }
                            , task: Just
                                { path: PF.buildPath
                                    { x: 3, y: 4 }
                                    []
                                , targetPosition: { x: 3, y: 4 }
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

            expectedCommands ∷ { | Commands CommandParameters }
            expectedCommands = Cmd.none
              { updateAnimation = Just
                  { animation:
                      { createEntity: []
                      , destroyEntity: []
                      , updateEntity:
                          [ { id: workerId
                            , sourcePosition: { x: 1, y: 2 }
                            , targetPosition: { x: 2, y: 3 }
                            }
                          ]
                      }
                  , gameStep: nextGameStep initialGameStep
                  }
              }

          pure
            { expectedCommands
            , messagePayload
            , modelExpectations
            , previousModel
            }

runScriptExecutedFailureScenario
  ∷ Int
  → ConfigM (Gen (HandlerFailureScenarioConfig ScriptExecutedPayload))
  → Aff Unit
runScriptExecutedFailureScenario =
  runFailureScenario CanvasInitialized.handle

runScriptExecutedSuccessScenario
  ∷ Int
  → ConfigM (Gen (HandlerSuccessScenarioConfig ScriptExecutedPayload))
  → Aff Unit
runScriptExecutedSuccessScenario =
  runSuccessScenario CanvasInitialized.handle

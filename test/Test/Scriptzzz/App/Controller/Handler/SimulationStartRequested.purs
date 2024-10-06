module Test.Scriptzzz.App.Controller.Handler.SimulationStartRequested
  ( spec
  ) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.Map as M
import Data.String.NonEmpty as NES
import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.SimulationStartRequested as SimulationStartRequested
import Scriptzzz.App.Message (SimulationStartRequestedPayload)
import Scriptzzz.App.Model (EditingModel, Model(..))
import Scriptzzz.App.Model.AnimationState
  ( AnimationState(..)
  , initialGameStep
  )
import Scriptzzz.Core (Id, Position, makeId)
import Scriptzzz.Game (Entity(..))
import Scriptzzz.Game as Game
import Test.Flame.Update.Handler
  ( ConfigM
  , runFailureScenario
  , runSuccessScenario
  )
import Test.Flame.Update.Handler as TH
import Test.Flame.Update.Handler.Scriptzzz
  ( HandlerFailureScenarioConfig
  , HandlerSuccessScenarioConfig
  , ModelAssertionConfig
  )
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.SimulationStartRequested" do
    it "fails when in a state different than Editing"
      $ runSimulationStartRequestedFailureScenario 10 do
          pure do
            previousModel ← arbitrary `suchThat` case _ of
              Editing _ →
                false

              _ → true

            let
              messagePayload ∷ SimulationStartRequestedPayload
              messagePayload = unit

              expectedErrorMessage ∷ String
              expectedErrorMessage =
                "Not in editing mode."

            pure
              { expectedErrorMessage
              , messagePayload
              , previousModel
              }

    it "switches from Editing to Simulationg state"
      $ runSimulationStartRequestedSuccessScenario 1 do
          pure do
            let
              entityId ∷ Id
              entityId = makeId (Proxy ∷ _ "foo")

              entityPosition ∷ Position
              entityPosition = { x: 1, y: 2 }

            editingModel ← arbitrary <#> \(model ∷ EditingModel) → model
              { gameSettings = model.gameSettings
                  { initialState = Game.State $ M.singleton
                      entityId
                      ( Worker
                          { position: entityPosition, task: Nothing }
                      )
                  }
              }

            let
              previousModel ∷ Model
              previousModel = Editing editingModel

              messagePayload ∷ SimulationStartRequestedPayload
              messagePayload = unit

              modelExpectations
                ∷ ∀ m
                . TH.Assert m
                ⇒ MonadAsk ModelAssertionConfig m
                ⇒ m Unit
              modelExpectations = do
                { nextModel } ← ask
                case nextModel of
                  Simulating simulatingModel → do
                    TH.assertEqual
                      { actual: simulatingModel.editor
                      , description: NES.nes
                          ( Proxy
                              ∷ _
                                  "previous editing mode state should be stored in simulating mode"
                          )
                      , expected: editingModel
                      }
                    TH.assertEqual
                      { actual: simulatingModel.gameState
                      , description: NES.nes
                          ( Proxy
                              ∷ _
                                  "game state should be loaded from editing mode game settings"
                          )
                      , expected: editingModel.gameSettings.initialState
                      }
                    TH.assertEqual
                      { actual: simulatingModel.animationState
                      , description: NES.nes
                          ( Proxy
                              ∷ _ "animation should be uninitialized"
                          )
                      , expected: Uninitialized
                      }

                  _ →
                    TH.fail $ NES.nes
                      (Proxy ∷ _ "not in simulating mode")

              expectedCommands ∷ { | Commands CommandParameters }
              expectedCommands = Cmd.none
                { updateAnimation = Just
                    { animation:
                        { createEntity:
                            [ { entityType: "worker"
                              , id: entityId
                              , position: entityPosition
                              }
                            ]
                        , destroyEntity: []
                        , updateEntity: []
                        }
                    , gameStep: initialGameStep
                    }
                }

            pure
              { expectedCommands
              , messagePayload
              , modelExpectations
              , previousModel
              }

runSimulationStartRequestedFailureScenario
  ∷ Int
  → ConfigM
      ( Gen
          (HandlerFailureScenarioConfig SimulationStartRequestedPayload)
      )
  → Aff Unit
runSimulationStartRequestedFailureScenario =
  runFailureScenario SimulationStartRequested.handle

runSimulationStartRequestedSuccessScenario
  ∷ Int
  → ConfigM
      ( Gen
          (HandlerSuccessScenarioConfig SimulationStartRequestedPayload)
      )
  → Aff Unit
runSimulationStartRequestedSuccessScenario =
  runSuccessScenario SimulationStartRequested.handle


module Test.Scriptzzz.App.Controller.Handler.SimulationStartRequested
  ( spec
  ) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.Map as M
import Data.String.NonEmpty as NES
import Data.Typelevel.Num (D4, d0, d1)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.SimulationStartRequested as SimulationStartRequested
import Scriptzzz.App.Message (SimulationStartRequestedPayload)
import Scriptzzz.App.Model (EditingModel, Model(..))
import Scriptzzz.App.Model.AnimationState
  ( AnimationState(..)
  , initialGameStep
  )
import Scriptzzz.Core (Id, Position, makeId, makePosition)
import Scriptzzz.Game (Entity(..))
import Scriptzzz.Game as Game
import Test.Flame.Update.Handler as TH
import Test.Flame.Update.Handler.Scriptzzz
  ( ModelAssertionConfig
  , RunFailureScenario
  , RunSuccessScenario
  )
import Test.QuickCheck (arbitrary)
import Test.Spec (Spec, describe, it)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.SimulationStartRequested" do
    it "fails when in a wrong mode" do
      runFailureScenario 10 do
        pure do
          previousModel ∷ Model D4 D4 ← arbitrary `suchThat` case _ of
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

    it "switches from Editing to Simulationg state" do
      runSuccessScenario 1 do
        pure do
          let
            entityId ∷ Id
            entityId = makeId (Proxy ∷ _ "foo")

            entityPosition ∷ Position D4 D4
            entityPosition = makePosition d0 d1

          editingModel ← arbitrary <#> \(model ∷ EditingModel D4 D4) →
            model
              { gameSettings = model.gameSettings
                  { initialState = Game.State $ M.singleton
                      entityId
                      ( Worker
                          { position: entityPosition, task: Nothing }
                      )
                  }
              }

          let
            previousModel ∷ Model D4 D4
            previousModel = Editing editingModel

            messagePayload ∷ SimulationStartRequestedPayload
            messagePayload = unit

            modelExpectations
              ∷ ∀ m
              . TH.Assert m
              ⇒ MonadAsk (ModelAssertionConfig D4 D4) m
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

            expectedCommands ∷ Cmd.Commands D4 D4
            expectedCommands = Cmd.none `Cmd.withUpdateAnimation`
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
  ⇒ RunFailureScenario w h SimulationStartRequestedPayload
runFailureScenario =
  TH.makeRunFailureScenario SimulationStartRequested.handle

runSuccessScenario
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ RunSuccessScenario w h SimulationStartRequestedPayload
runSuccessScenario =
  TH.makeRunSuccessScenario SimulationStartRequested.handle


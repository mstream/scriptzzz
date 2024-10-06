module Test.Scriptzzz.App.Controller.Handler.SimulationStopRequested
  ( spec
  ) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.String.NonEmpty as NES
import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.SimulationStopRequested as SimulationStopRequested
import Scriptzzz.App.Message (SimulationStopRequestedPayload)
import Scriptzzz.App.Model (Model(..), SimulatingModel)
import Scriptzzz.Core (Id, Position, makeId)
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
  describe "Scriptzz.App.Controller.Handler.SimulationStopRequested" do
    it "fails when in a state different than Simulating"
      $ runSimulationStopRequestedFailureScenario 10 do
          pure do
            previousModel ← arbitrary `suchThat` case _ of
              Simulating _ →
                false

              _ → true

            let
              messagePayload ∷ SimulationStopRequestedPayload
              messagePayload = unit

              expectedErrorMessage ∷ String
              expectedErrorMessage =
                "Not in simulating mode."

            pure
              { expectedErrorMessage
              , messagePayload
              , previousModel
              }

    it "switches from Simulating to Editing state"
      $ runSimulationStopRequestedSuccessScenario 1 do
          pure do
            let
              entityId ∷ Id
              entityId = makeId (Proxy ∷ _ "foo")

              entityPosition ∷ Position
              entityPosition = { x: 1, y: 2 }

            editingModel ← arbitrary

            simulatingModel ← arbitrary <#> \(model ∷ SimulatingModel) →
              model
                { editor = editingModel }

            let
              previousModel ∷ Model
              previousModel = Simulating simulatingModel

              messagePayload ∷ SimulationStopRequestedPayload
              messagePayload = unit

              modelExpectations
                ∷ ∀ m
                . TH.Assert m
                ⇒ MonadAsk ModelAssertionConfig m
                ⇒ m Unit
              modelExpectations = do
                { nextModel } ← ask
                case nextModel of
                  Editing editingModel → do
                    TH.assertEqual
                      { actual: simulatingModel.editor
                      , description: NES.nes
                          ( Proxy
                              ∷ _
                                  "previous editing mode state should be stored in simulating mode"
                          )
                      , expected: editingModel
                      }

                  _ →
                    TH.fail $ NES.nes
                      (Proxy ∷ _ "not in editing mode")

              expectedCommands ∷ { | Commands CommandParameters }
              expectedCommands = Cmd.none

            pure
              { expectedCommands
              , messagePayload
              , modelExpectations
              , previousModel
              }

runSimulationStopRequestedFailureScenario
  ∷ Int
  → ConfigM
      ( Gen
          (HandlerFailureScenarioConfig SimulationStopRequestedPayload)
      )
  → Aff Unit
runSimulationStopRequestedFailureScenario =
  runFailureScenario SimulationStopRequested.handle

runSimulationStopRequestedSuccessScenario
  ∷ Int
  → ConfigM
      ( Gen
          (HandlerSuccessScenarioConfig SimulationStopRequestedPayload)
      )
  → Aff Unit
runSimulationStopRequestedSuccessScenario =
  runSuccessScenario SimulationStopRequested.handle


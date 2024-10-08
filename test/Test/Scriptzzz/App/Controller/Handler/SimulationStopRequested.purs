module Test.Scriptzzz.App.Controller.Handler.SimulationStopRequested
  ( spec
  ) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.String.NonEmpty as NES
import Data.Typelevel.Num (D4)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.SimulationStopRequested as SimulationStopRequested
import Scriptzzz.App.Message (SimulationStopRequestedPayload)
import Scriptzzz.App.Model (Model(..), SimulatingModel)
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
  describe "Scriptzz.App.Controller.Handler.SimulationStopRequested" do
    it "fails when in wrong mode"
      $ runFailureScenario 10 do
          pure do
            previousModel ∷ Model D4 D4 ← arbitrary `suchThat` case _ of
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
      $ runSuccessScenario 1 do
          pure do
            editingModel ← arbitrary

            simulatingModel ← arbitrary <#>
              \(model ∷ SimulatingModel D4 D4) →
                model
                  { editor = editingModel }

            let
              previousModel ∷ Model D4 D4
              previousModel = Simulating simulatingModel

              messagePayload ∷ SimulationStopRequestedPayload
              messagePayload = unit

              modelExpectations
                ∷ ∀ m
                . TH.Assert m
                ⇒ MonadAsk (ModelAssertionConfig D4 D4) m
                ⇒ m Unit
              modelExpectations = do
                { nextModel } ← ask
                case nextModel of
                  Editing actualEditingModel → do
                    TH.assertEqual
                      { actual: actualEditingModel
                      , description: NES.nes
                          ( Proxy
                              ∷ _
                                  "previous editing mode state should be stored in simulating mode"
                          )
                      , expected: simulatingModel.editor
                      }

                  _ →
                    TH.fail $ NES.nes
                      (Proxy ∷ _ "not in editing mode")

              expectedCommands ∷ Cmd.Commands D4 D4
              expectedCommands = Cmd.none

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
  ⇒ RunFailureScenario w h SimulationStopRequestedPayload
runFailureScenario =
  TH.makeRunFailureScenario SimulationStopRequested.handle

runSuccessScenario
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ RunSuccessScenario w h SimulationStopRequestedPayload
runSuccessScenario =
  TH.makeRunSuccessScenario SimulationStopRequested.handle


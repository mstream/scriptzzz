module Test.Scriptzzz.App.Controller.Handler.AnimationUpdated (spec) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.String.NonEmpty as NES
import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.AnimationUpdated as AnimationUpdated
import Scriptzzz.App.Message (AnimationUpdatedPayload)
import Scriptzzz.App.Model (Model(..), SimulatingModel)
import Scriptzzz.App.Model.AnimationState (AnimationState(..))
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
  describe "Scriptzz.App.Controller.Handler.AnimationUpdated" do
    it "fails when in a state different than Simulating"
      $ runAnimationUpdatedFailureScenario 10 do
          t0 ← TH.nextTimeTickTimestamp
          t1 ← TH.nextTimeTickTimestamp

          pure do
            previousModel ← arbitrary `suchThat` case _ of
              Simulating _ →
                false

              _ →
                true

            gameStep ← arbitrary

            let
              messagePayload ∷ AnimationUpdatedPayload
              messagePayload =
                { executionFinishTime: t1
                , executionStartTime: t0
                , gameStep
                }

              expectedErrorMessage ∷ String
              expectedErrorMessage =
                "Not in simulating mode."

            pure
              { expectedErrorMessage
              , messagePayload
              , previousModel
              }

    it "Updates animation state and schedules another script execution"
      $ runAnimationUpdatedSuccessScenario 1 do
          t0 ← TH.nextTimeTickTimestamp
          t1 ← TH.nextTimeTickTimestamp

          pure do
            simulatingModel ∷ SimulatingModel ← arbitrary
            gameStep ← arbitrary

            let
              previousModel ∷ Model
              previousModel = Simulating simulatingModel
                { animationState = Updating { gameStep } }

              messagePayload ∷ AnimationUpdatedPayload
              messagePayload =
                { executionFinishTime: t1
                , executionStartTime: t0
                , gameStep
                }

              modelExpectations
                ∷ ∀ m
                . TH.Assert m
                ⇒ MonadAsk ModelAssertionConfig m
                ⇒ m Unit
              modelExpectations = do
                { nextModel } ← ask
                case nextModel of
                  Simulating actualSimulatingModel →
                    case actualSimulatingModel.animationState of
                      Updated actualUpdatedModel →
                        TH.assertEqual
                          { actual: actualUpdatedModel.gameStep
                          , description: NES.nes
                              ( Proxy
                                  ∷ _
                                      "animation state should be updated with a game steps coming from the message"
                              )
                          , expected: messagePayload.gameStep
                          }
                      _ →
                        TH.fail $ NES.nes
                          (Proxy ∷ _ "animation state is not updated")

                  _ →
                    TH.fail $ NES.nes
                      (Proxy ∷ _ "not in simulation mode")

              expectedCommands ∷ { | Commands CommandParameters }
              expectedCommands = Cmd.none
                { executeScript = Just simulatingModel.editor.script
                }

            pure
              { expectedCommands
              , messagePayload
              , modelExpectations
              , previousModel
              }

runAnimationUpdatedFailureScenario
  ∷ Int
  → TH.ConfigM
      (Gen (HandlerFailureScenarioConfig AnimationUpdatedPayload))
  → Aff Unit
runAnimationUpdatedFailureScenario =
  TH.runFailureScenario AnimationUpdated.handle

runAnimationUpdatedSuccessScenario
  ∷ Int
  → TH.ConfigM
      (Gen (HandlerSuccessScenarioConfig AnimationUpdatedPayload))
  → Aff Unit
runAnimationUpdatedSuccessScenario =
  TH.runSuccessScenario AnimationUpdated.handle


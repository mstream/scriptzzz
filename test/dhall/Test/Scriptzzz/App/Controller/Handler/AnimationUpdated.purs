module Test.Scriptzzz.App.Controller.Handler.AnimationUpdated (spec) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.String.NonEmpty as NES
import Data.Typelevel.Num (D4)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.AnimationUpdated as AnimationUpdated
import Scriptzzz.App.Message (AnimationUpdatedPayload)
import Scriptzzz.App.Model (Model(..), SimulatingModel)
import Scriptzzz.App.Model.AnimationState (AnimationState(..))
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
  describe "Scriptzz.App.Controller.Handler.AnimationUpdated" do
    it "fails when in a wrong mode" do
      runFailureScenario 10 do
        t0 ← TH.nextTimeTickTimestamp
        t1 ← TH.nextTimeTickTimestamp

        pure do
          previousModel ∷ Model D4 D4 ← arbitrary `suchThat` case _ of
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

    it "update animation" do
      runSuccessScenario 1 do
        t0 ← TH.nextTimeTickTimestamp
        t1 ← TH.nextTimeTickTimestamp

        pure do
          simulatingModel ∷ SimulatingModel D4 D4 ← arbitrary
          gameStep ← arbitrary

          let
            previousModel ∷ Model D4 D4
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
              ⇒ MonadAsk (ModelAssertionConfig D4 D4) m
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

            expectedCommands ∷ Cmd.Commands D4 D4
            expectedCommands = Cmd.none `Cmd.withExecuteScript`
              simulatingModel.editor.script

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
  ⇒ RunFailureScenario w h AnimationUpdatedPayload
runFailureScenario =
  TH.makeRunFailureScenario AnimationUpdated.handle

runSuccessScenario
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ RunSuccessScenario w h AnimationUpdatedPayload
runSuccessScenario =
  TH.makeRunSuccessScenario AnimationUpdated.handle


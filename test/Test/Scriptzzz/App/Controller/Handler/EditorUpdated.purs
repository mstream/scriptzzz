module Test.Scriptzzz.App.Controller.Handler.EditorUpdated (spec) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.String.NonEmpty as NES
import Data.Typelevel.Num (D4)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.EditorUpdated as EditorUpdated
import Scriptzzz.App.Message (EditorUpdatedPayload)
import Scriptzzz.App.Model (Model(..))
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
  describe "Scriptzz.App.Controller.Handler.EditorUpdated" do
    it "fails when in wrong mode" do
      runFailureScenario 10 do
        pure do
          previousModel ∷ Model D4 D4 ← arbitrary `suchThat` case _ of
            Editing _ →
              false

            _ →
              true

          messagePayload ← arbitrary

          let
            expectedErrorMessage ∷ String
            expectedErrorMessage = "Not in editing mode."

          pure
            { expectedErrorMessage
            , messagePayload
            , previousModel
            }

    it "updates script" do
      runSuccessScenario 10 do
        pure do
          previousModel ∷ Model D4 D4 ← arbitrary `suchThat` case _ of
            Editing _ →
              true

            _ →
              false

          messagePayload ← arbitrary

          let
            modelExpectations
              ∷ ∀ h m w
              . Pos h
              ⇒ Pos w
              ⇒ TH.Assert m
              ⇒ MonadAsk (ModelAssertionConfig w h) m
              ⇒ m Unit
            modelExpectations = do
              { nextModel } ← ask
              case nextModel of
                Editing editingModel →
                  TH.assertEqual
                    { actual: editingModel.script
                    , description: NES.nes
                        ( Proxy
                            ∷ _
                                "editing mode script should be updated with a script from the message"
                        )
                    , expected: messagePayload
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

runFailureScenario
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ RunFailureScenario w h EditorUpdatedPayload
runFailureScenario = TH.makeRunFailureScenario EditorUpdated.handle

runSuccessScenario
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ RunSuccessScenario w h EditorUpdatedPayload
runSuccessScenario = TH.makeRunSuccessScenario EditorUpdated.handle


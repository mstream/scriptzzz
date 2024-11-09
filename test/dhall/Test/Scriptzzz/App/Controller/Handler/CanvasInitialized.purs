module Test.Scriptzzz.App.Controller.Handler.CanvasInitialized (spec) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.String.NonEmpty as NES
import Data.Typelevel.Num (D4)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.CanvasInitialized as CanvasInitialized
import Scriptzzz.App.Message (CanvasInitializedPayload)
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
  describe "Scriptzz.App.Controller.Handler.CanvasInitialized" do
    it "fails when in a state different than CanvasInitializing"
      $ runFailureScenario 10 do
          pure do
            previousModel ∷ Model D4 D4 ← arbitrary `suchThat` case _ of
              CanvasInitializing _ →
                false

              _ → true

            let
              messagePayload ∷ CanvasInitializedPayload
              messagePayload = unit

              expectedErrorMessage ∷ String
              expectedErrorMessage =
                "Canvas has already been initialized."

            pure
              { expectedErrorMessage
              , messagePayload
              , previousModel
              }

    it "switches from CanvasInitializing to Editing state"
      $ runSuccessScenario 1 do
          pure do
            gameEnvironment ← arbitrary

            let
              previousModel ∷ Model D4 D4
              previousModel = CanvasInitializing gameEnvironment

              messagePayload ∷ CanvasInitializedPayload
              messagePayload = unit

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
                                  "editing mode should have no last script execution info set"
                          )
                      , expected: Nothing
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
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ RunFailureScenario w h CanvasInitializedPayload
runFailureScenario = TH.makeRunFailureScenario CanvasInitialized.handle

runSuccessScenario
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ RunSuccessScenario w h CanvasInitializedPayload
runSuccessScenario = TH.makeRunSuccessScenario CanvasInitialized.handle


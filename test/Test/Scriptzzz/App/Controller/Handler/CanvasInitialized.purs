module Test.Scriptzzz.App.Controller.Handler.CanvasInitialized (spec) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.String.NonEmpty as NES
import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.CanvasInitialized as CanvasInitialized
import Scriptzzz.App.Message (CanvasInitializedPayload)
import Scriptzzz.App.Model (Model(..))
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
  describe "Scriptzz.App.Controller.Handler.CanvasInitialized" do
    it "fails when in a state different than CanvasInitializing"
      $ runCanvasInitializedFailureScenario 10 do
          pure do
            previousModel ← arbitrary `suchThat` case _ of
              CanvasInitializing →
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
      $ runCanvasInitializedSuccessScenario 1 do
          pure do
            let
              previousModel ∷ Model
              previousModel = CanvasInitializing

              messagePayload ∷ CanvasInitializedPayload
              messagePayload = unit

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
                                  "editing mode should have no last script execution info set"
                          )
                      , expected: Nothing
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

runCanvasInitializedFailureScenario
  ∷ Int
  → ConfigM
      (Gen (HandlerFailureScenarioConfig CanvasInitializedPayload))
  → Aff Unit
runCanvasInitializedFailureScenario =
  runFailureScenario CanvasInitialized.handle

runCanvasInitializedSuccessScenario
  ∷ Int
  → ConfigM
      (Gen (HandlerSuccessScenarioConfig CanvasInitializedPayload))
  → Aff Unit
runCanvasInitializedSuccessScenario =
  runSuccessScenario CanvasInitialized.handle


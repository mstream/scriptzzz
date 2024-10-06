module Test.Scriptzzz.App.Controller.Handler.EditorUpdated (spec) where

import Scriptzzz.Prelude

import Control.Monad.Gen (suchThat)
import Data.String.NonEmpty as NES
import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.EditorUpdated as EditorUpdated
import Scriptzzz.App.Message (EditorUpdatedPayload)
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
  describe "Scriptzz.App.Controller.Handler.EditorUpdated" do
    it "fails when in a state different than Editing"
      $ runEditorUpdatedFailureScenario 10 do
          pure do
            previousModel ← arbitrary `suchThat` case _ of
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

    it "should update script"
      $ runEditorUpdatedSuccessScenario 10 do
          pure do
            previousModel ← arbitrary `suchThat` case _ of
              Editing _ →
                true

              _ →
                false

            messagePayload ← arbitrary

            let
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

              expectedCommands ∷ { | Commands CommandParameters }
              expectedCommands = Cmd.none

            pure
              { expectedCommands
              , messagePayload
              , modelExpectations
              , previousModel
              }

runEditorUpdatedFailureScenario
  ∷ Int
  → ConfigM (Gen (HandlerFailureScenarioConfig EditorUpdatedPayload))
  → Aff Unit
runEditorUpdatedFailureScenario = runFailureScenario
  EditorUpdated.handle

runEditorUpdatedSuccessScenario
  ∷ Int
  → ConfigM (Gen (HandlerSuccessScenarioConfig EditorUpdatedPayload))
  → Aff Unit
runEditorUpdatedSuccessScenario =
  runSuccessScenario EditorUpdated.handle


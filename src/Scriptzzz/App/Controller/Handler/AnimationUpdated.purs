module Scriptzzz.App.Controller.Handler.AnimationUpdated (handle) where

import Scriptzzz.Prelude

import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler as Handler
import Scriptzzz.App.Message (AnimationUpdatedPayload)
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.App.Model.AnimationState (AnimationState(..))

type Handle =
  Handler.HandleMessage
    Model
    { | Commands CommandParameters }
    AnimationUpdatedPayload

handle ∷ Handle
handle model message = case model of
  Simulating simulatingModel →
    let
      newModel ∷ Model
      newModel = Simulating simulatingModel
        { animationState = Updated { gameStep: message.gameStep } }

      commands ∷ { | Commands CommandParameters }
      commands = Cmd.none
        { executeScript = Just $ simulatingModel.editor.script
        }

    in
      Handler.success { commands, newModel }

  _ →
    Handler.failure "Not in simulating mode."


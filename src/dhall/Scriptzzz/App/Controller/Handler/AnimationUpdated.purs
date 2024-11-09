module Scriptzzz.App.Controller.Handler.AnimationUpdated (handle) where

import Scriptzzz.Prelude

import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleScriptzzzMessage)
import Scriptzzz.App.Controller.Handler as Handler
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.App.Model.AnimationState (AnimationState(..))

handle
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ HandleScriptzzzMessage w h Msg.AnimationUpdatedPayload
handle model message = case model of
  Simulating simulatingModel →
    let
      newModel ∷ Model w h
      newModel = Simulating simulatingModel
        { animationState = Updated { gameStep: message.gameStep } }

      commands ∷ Cmd.Commands w h 
      commands = Cmd.none `Cmd.withExecuteScript` simulatingModel.editor.script

    in
      Handler.success { commands, newModel }

  _ →
    Handler.failure "Not in simulating mode."


module Scriptzzz.App.Controller.Handler.SimulationStopRequested
  ( handle
  ) where

import Data.Typelevel.Num (class Pos)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleScriptzzzMessage)
import Scriptzzz.App.Controller.Handler as Handler
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))

handle
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ HandleScriptzzzMessage w h Msg.SimulationStopRequestedPayload
handle model _ = case model of
  Simulating simulatingModel →
    let
      newModel ∷ Model w h
      newModel = Editing simulatingModel.editor

      commands ∷ Cmd.Commands w h
      commands = Cmd.none

    in
      Handler.success { commands, newModel }

  _ →
    Handler.failure "Not in simulating mode."


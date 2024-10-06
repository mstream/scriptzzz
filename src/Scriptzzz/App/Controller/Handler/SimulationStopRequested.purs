module Scriptzzz.App.Controller.Handler.SimulationStopRequested
  ( handle
  ) where

import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler as Handler
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))

type Handle =
  Handler.HandleMessage
    Model
    { | Commands CommandParameters }
    Msg.SimulationStopRequestedPayload

handle ∷ Handle
handle model _ = case model of
  Simulating simulatingModel →
    let
      newModel ∷ Model
      newModel = Editing simulatingModel.editor

      commands ∷ { | Commands CommandParameters }
      commands = Cmd.none

    in
      Handler.success { commands, newModel }

  _ →
    Handler.failure "Not in simulating mode."


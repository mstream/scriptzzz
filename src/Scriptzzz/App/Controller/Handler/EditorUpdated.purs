module Scriptzzz.App.Controller.Handler.EditorUpdated (handle) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Scriptzzz.App.Command (Commands, CommandParameters)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleMessage)
import Scriptzzz.App.Message (EditorUpdatedPayload)
import Scriptzzz.App.Model (Model(..))

type Handle = 
  HandleMessage 
    Model
    { | Commands CommandParameters }
    EditorUpdatedPayload

handle ∷ Handle
handle model script = case model of
  Editing editingModel ->
    Right $ Editing editingModel {script = script} /\ Cmd.none 

  _ ->
    Left "Not in editing mode."

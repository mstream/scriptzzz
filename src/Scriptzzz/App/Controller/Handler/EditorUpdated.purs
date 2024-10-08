module Scriptzzz.App.Controller.Handler.EditorUpdated (handle) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Pos)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleScriptzzzMessage)
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))

handle ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ HandleScriptzzzMessage w h Msg.EditorUpdatedPayload
handle model script = case model of
  Editing editingModel ->
    Right $ Editing editingModel {script = script} /\ Cmd.none 

  _ ->
    Left "Not in editing mode."

module Scriptzzz.App.Controller.Handler.TimeUpdated (handle) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Scriptzzz.App.Command (Commands, CommandParameters)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleMessage)
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model)

type Handle = 
  HandleMessage 
    Model
    { | Commands CommandParameters }
    Msg.TimeUpdatedPayload

handle ∷ Handle
handle model _ = Right $ model /\ Cmd.none

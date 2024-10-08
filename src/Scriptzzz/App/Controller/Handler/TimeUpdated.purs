module Scriptzzz.App.Controller.Handler.TimeUpdated (handle) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Pos)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleScriptzzzMessage)
import Scriptzzz.App.Message as Msg

handle
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ HandleScriptzzzMessage w h Msg.TimeUpdatedPayload
handle model _ = Right $ model /\ Cmd.none

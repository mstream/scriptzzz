module Scriptzzz.App (makeApplication) where

import Scriptzzz.Prelude

import Flame (Application, Subscription)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller (DebugLevel, init, update)
import Scriptzzz.App.Message (Message)
import Scriptzzz.App.Model (Model)
import Scriptzzz.App.View (view)
import Scriptzzz.PathFinding as PF

makeApplication
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Cmd.CommandExecutors w h Aff
  → DebugLevel w h
  → Application (Model w h) (Message w h)
makeApplication commandExecutors debugLevel =
  { init: init PF.emptyObstacleMatrix
  , subscribe
  , update: update commandExecutors debugLevel
  , view
  }

subscribe ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Array (Subscription (Message w h))
subscribe = []


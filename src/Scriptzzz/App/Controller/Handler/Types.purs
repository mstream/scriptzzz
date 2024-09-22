module Scriptzzz.App.Controller.Handler.Types (HandleMessage) where

import Data.Either.Nested (type (\/))
import Data.Tuple.Nested (type (/\))
import Scriptzzz.App.Command (Commands)
import Scriptzzz.App.Model (Model)
import Scriptzzz.Game as Game

type HandleMessage a =
  Model → a → String \/ Model /\ Commands Game.Commands

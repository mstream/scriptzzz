module Scriptzzz.App (app) where

import Scriptzzz.Prelude

import Flame (Application, Subscription)
import Scriptzzz.App.Command (CommandExecutor, Commands)
import Scriptzzz.App.Controller (DebugLevel, init, update)
import Scriptzzz.App.Message (Message)
import Scriptzzz.App.Model (Model)
import Scriptzzz.App.View (view)

app ∷ { | Commands (CommandExecutor Aff) } → DebugLevel -> Application Model Message
app commandExecutors debugLevel =
  { init, subscribe, update: update commandExecutors debugLevel, view }

subscribe ∷ Array (Subscription Message)
subscribe = []


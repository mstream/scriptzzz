module Scriptzzz.App (app) where

import Flame (Application, Subscription)
import Scriptzzz.App.Controller (init, update)
import Scriptzzz.App.Message (Message)
import Scriptzzz.App.Model (Model)
import Scriptzzz.App.View (view)

app ∷ Application Model Message
app = { init, subscribe, update, view }

subscribe ∷ Array (Subscription Message)
subscribe = []


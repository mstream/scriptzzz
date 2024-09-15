module Scriptzzz.Main where

import Prelude

import Effect (Effect)
import Effect.Now (now)
import Effect.Timer (setInterval)
import Flame (QuerySelector(..), mount)
import Scriptzzz.App (app)
import Scriptzzz.App.Message (Message(..))
import Scriptzzz.Global (appId, sendMessage)

main ∷ Effect Unit
main = do
  mount (QuerySelector "body") appId app
  void $ setInterval 100 do
    currentTime ← now
    sendMessage $ TimeUpdated currentTime

module Scriptzzz.Main where

import Scriptzzz.Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class.Console as Console
import Flame (QuerySelector(..), mount)
import Scriptzzz.App (app)
import Scriptzzz.App.Command (CommandExecutor, Commands)
import Scriptzzz.App.Controller (DebugLevel(..))
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.Core (timestamp)
import Scriptzzz.Global (appId, sendMessage)
import Scriptzzz.Sandbox as Sandbox
import Web.HTML as H
import Web.HTML.Window as HW

main ∷ Effect Unit
main = do
  window ← H.window
  mount
    (QuerySelector "body")
    appId
    (app commandExecutors (Full showSimplifiedModel))
  {- initializeTimeUpdateProducer window -}
  where
  initializeTimeUpdateProducer ∷ H.Window → Effect Unit
  initializeTimeUpdateProducer window = void $ HW.requestAnimationFrame
    ( do
        time ← timestamp <$> now
        message ← Msg.createWithTimestamp $ Msg.TimeUpdated time
        sendMessage message
        void $ initializeTimeUpdateProducer window
    )
    window

  commandExecutors ∷ { | Commands (CommandExecutor Aff) }
  commandExecutors =
    { executeScript: Sandbox.runProgram 
        (Sandbox.runSandboxM "./sandbox.js") 
        (Milliseconds 500.0)
    , logDebug: Console.debug
    , logError: Console.error
    , updateAnimation: \animation → do
        pure mempty
    }

showSimplifiedModel ∷ Model → String
showSimplifiedModel = case _ of
  CanvasInitializing →
    "canvas initializing..."
  Editing _ →
    "editing mode"
  Simulating _ →
    "simulating mode"

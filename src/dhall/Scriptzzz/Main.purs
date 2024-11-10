module Scriptzzz.Main where

import Scriptzzz.Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Typelevel.Num (D16)
import Effect.Class.Console as Console
import Flame (Application, QuerySelector(..), mount)
import Scriptzzz.App (makeApplication)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller (DebugLevel(..))
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.Canvas as Canvas
import Scriptzzz.Core (timestamp)
import Scriptzzz.Global (appId, sendMessage)
import Scriptzzz.Sandbox as Sandbox
import Web.HTML as H
import Web.HTML.Window as HW

main ∷ Effect Unit
main = do
  let
    application /\ timeUpdateProducer = initialize @D16

  mount (QuerySelector "body") appId application

{- H.window >>= timeUpdateProducer -}

initialize
  ∷ ∀ @d
  . Pos d
  ⇒ Application (Model d d) (Msg.Message d d) /\
      (H.Window → Effect Unit)
initialize = application /\ timeUpdateProducer
  where
  application ∷ Application (Model d d) (Msg.Message d d)
  application = makeApplication
    commandExecutors
    (Full showSimplifiedModel)

  timeUpdateProducer ∷ H.Window → Effect Unit
  timeUpdateProducer window = void $ HW.requestAnimationFrame
    ( do
        time ← timestamp <$> now
        message ∷ Msg.Message d d ← Msg.createWithTimestamp $
          Msg.TimeUpdated time
        sendMessage message
        timeUpdateProducer window
    )
    window

commandExecutors ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Cmd.CommandExecutors w h Aff
commandExecutors =
  { executeScript: Sandbox.runProgram
      (Sandbox.runSandboxM "./sandbox.js")
      (Milliseconds 500.0)
  , logDebug: Console.debug
  , logError: Console.error
  , updateAnimation: \{ animation } → do
      parTraverse_
        ( \{ entityType, id, position } →
            Canvas.createEntity id entityType position
        )
        animation.createEntity

      parTraverse_
        ( \{ id, targetPosition } →
            Canvas.updateEntityPosition id targetPosition
        )
        animation.updateEntity

      parTraverse_
        ( \{ id } →
            Canvas.destroyEntity id
        )
        animation.destroyEntity
      pure { errors: [] }
  }

showSimplifiedModel ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Model w h → String
showSimplifiedModel = case _ of
  CanvasInitializing _ →
    "canvas initializing..."
  Editing _ →
    "editing mode"
  Simulating _ →
    "simulating mode"

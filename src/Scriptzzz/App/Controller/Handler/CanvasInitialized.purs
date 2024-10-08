module Scriptzzz.App.Controller.Handler.CanvasInitialized (handle) where

import Scriptzzz.Prelude

import Data.Map as M
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleScriptzzzMessage)
import Scriptzzz.App.Controller.Handler as Handler
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.Canvas.Animation as Animation
import Scriptzzz.Core (Position, makeId)
import Scriptzzz.Game (Entity(..))
import Scriptzzz.Game as Game
import Test.QuickCheck (mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))

handle
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ HandleScriptzzzMessage w h Msg.CanvasInitializedPayload
handle model _ = case model of
  CanvasInitializing gameEnvironment →
    let
      newModel ∷ Model w h
      newModel = Editing
        { gameSettings:
            { environment: gameEnvironment
            , initialState: initialGameState
            , restartOnScriptChange: false
            , stopOnError: false
            , targetFps: Animation.tenFramesPerSecond
            }
        , lastScriptExecution: Nothing
        , script: mempty
        }

      commands ∷ Cmd.Commands w h
      commands = Cmd.none

    in
      Handler.success { commands, newModel }

  _ →
    Handler.failure "Canvas has already been initialized."

initialGameState ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Game.State w h
initialGameState =
  let
    position ∷ Position w h
    position = evalGen arbitrary { newSeed: mkSeed 0, size: one }

    worker ∷ Game.Entity w h
    worker = Worker
      { position
      , task: Nothing
      }
  in
    Game.State $ M.singleton
      (makeId (Proxy ∷ _ "foo"))
      worker

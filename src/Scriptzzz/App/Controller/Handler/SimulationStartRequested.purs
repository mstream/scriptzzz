module Scriptzzz.App.Controller.Handler.SimulationStartRequested
  ( handle
  ) where

import Scriptzzz.Prelude

import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleScriptzzzMessage)
import Scriptzzz.App.Controller.Handler as Handler
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.App.Model.AnimationState
  ( AnimationState(..)
  , initialGameStep
  )
import Scriptzzz.Canvas.Animation as Animation
import Scriptzzz.Game as Game

handle
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ HandleScriptzzzMessage w h Msg.SimulationStopRequestedPayload
handle model _ = case model of
  Editing editingModel →
    let
      gameState ∷ Game.State w h
      gameState = editingModel.gameSettings.initialState

      newModel ∷ Model w h
      newModel = Simulating
        { animationState: Uninitialized
        , editor: editingModel
        , gameLogs: mempty
        , gameState
        }

      commands ∷ Cmd.Commands w h
      commands = Cmd.none `Cmd.withUpdateAnimation`
        { animation: Animation.animate
            Game.blankState
            gameState
        , gameStep: initialGameStep
        }

    in
      Handler.success { commands, newModel }

  _ →
    Handler.failure "Not in editing mode."


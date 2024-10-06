module Scriptzzz.App.Controller.Handler.SimulationStartRequested
  ( handle
  ) where

import Scriptzzz.Prelude

import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler as Handler
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.App.Model.AnimationState
  ( AnimationState(..)
  , initialGameStep
  )
import Scriptzzz.Canvas.Animation as Animation
import Scriptzzz.Game as Game

type Handle =
  Handler.HandleMessage
    Model
    { | Commands CommandParameters }
    Msg.SimulationStartRequestedPayload

handle ∷ Handle
handle model _ = case model of
  Editing editingModel →
    let
      gameState ∷ Game.State
      gameState = editingModel.gameSettings.initialState

      newModel ∷ Model
      newModel = Simulating
        { animationState: Uninitialized
        , editor: editingModel
        , gameLogs: mempty
        , gameState
        }

      commands ∷ { | Commands CommandParameters }
      commands = Cmd.none
        { updateAnimation = Just
            { animation: Animation.animate
                Game.blankState
                gameState
            , gameStep: initialGameStep
            }
        }

    in
      Handler.success { commands, newModel }

  _ →
    Handler.failure "Not in editing mode."


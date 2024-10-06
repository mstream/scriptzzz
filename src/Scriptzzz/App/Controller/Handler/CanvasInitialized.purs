module Scriptzzz.App.Controller.Handler.CanvasInitialized (handle) where

import Scriptzzz.Prelude

import Data.Map as M
import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler as Handler
import Scriptzzz.App.Message (CanvasInitializedPayload)
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.Canvas.Animation as Animation
import Scriptzzz.Core (makeId)
import Scriptzzz.Game (Entity(..))
import Scriptzzz.Game as Game
import Scriptzzz.PathFinding as PF
import Type.Proxy (Proxy(..))

type Handle =
  Handler.HandleMessage
    Model
    { | Commands CommandParameters }
    CanvasInitializedPayload

handle ∷ Handle
handle model _ = case model of
  CanvasInitializing →
    let
      newModel ∷ Model
      newModel = Editing
        { gameSettings:
            { environment:
                { obstacleMatrix: PF.emptyObstacleMatrix }
            , initialState: initialGameState
            , restartOnScriptChange: false
            , stopOnError: false
            , targetFps: Animation.tenFramesPerSecond
            }
        , lastScriptExecution: Nothing
        , script: mempty
        }

      commands ∷ { | Commands CommandParameters }
      commands = Cmd.none

    in
      Handler.success { commands, newModel }

  _ →
    Handler.failure "Canvas has already been initialized."

initialGameState ∷ Game.State
initialGameState = Game.State $ M.singleton
  (makeId (Proxy ∷ _ "foo"))
  (Worker { position: { x: 15, y: 15 }, task: Nothing })


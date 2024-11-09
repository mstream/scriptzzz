module Scriptzzz.App.Controller.Handler.ScriptExecuted (handle) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Pos)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleScriptzzzMessage)
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.App.Model.AnimationState
  ( AnimationState(..)
  , GameStep
  , nextGameStep
  )
import Scriptzzz.Canvas.Animation (Animation, animate)
import Scriptzzz.Game as Game
import Scriptzzz.Sandbox (ExecutionResult(..))

handle
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ HandleScriptzzzMessage w h (Msg.ScriptExecutedPayload w h)
handle model message = case model of
  Simulating simulatingModel →
    case simulatingModel.animationState of
      Uninitialized →
        Left "Animation has not been initialized yet."

      Updating _ →
        Left "Previous animation has not finished."

      Updated updatedModel →
        let
          gameStep ∷ GameStep
          gameStep = nextGameStep updatedModel.gameStep
        in
          case message.executionResult of
            Success gameCommands →
              let
                newGameState /\ newGameLogs = Game.update
                  simulatingModel.editor.gameSettings.environment
                  gameCommands
                  simulatingModel.gameState

                animation ∷ Animation w h
                animation = animate
                  simulatingModel.gameState
                  newGameState

                newModel ∷ Model w h
                newModel = Simulating simulatingModel
                  { animationState = Updating { gameStep }
                  , editor = simulatingModel.editor
                      { lastScriptExecution = Just
                          { finishTime: message.executionFinishTime
                          , result: Success gameCommands
                          , startTime: message.executionStartTime
                          }
                      }
                  , gameLogs = simulatingModel.gameLogs <> newGameLogs
                  , gameState = newGameState
                  }

                commands ∷ Cmd.Commands w h
                commands = Cmd.none `Cmd.withUpdateAnimation`
                  { animation, gameStep }

              in
                Right $ newModel /\ commands

            failedExecutionResult →
              Right $
                if simulatingModel.editor.gameSettings.stopOnError then
                  Editing simulatingModel.editor
                    { lastScriptExecution = Just
                        { finishTime: message.executionFinishTime
                        , result: failedExecutionResult
                        , startTime: message.executionStartTime
                        }
                    } /\
                    Cmd.none
                else
                  Simulating simulatingModel
                    { animationState = Updated { gameStep }
                    , editor = simulatingModel.editor
                        { lastScriptExecution = Just
                            { finishTime: message.executionFinishTime
                            , result: failedExecutionResult
                            , startTime: message.executionStartTime
                            }
                        }
                    } /\
                    Cmd.none

  _ →
    Left "Not in simulating mode."


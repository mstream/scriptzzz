module Scriptzzz.App.Controller.Handler.ScriptExecuted (handle) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler (HandleMessage)
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

type Handle =
  HandleMessage
    Model
    { | Commands CommandParameters }
    Msg.ScriptExecutedPayload

handle ∷ Handle
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
            Success commands →
              let
                newGameState /\ newGameLogs = Game.update
                  simulatingModel.editor.gameSettings.environment
                  commands
                  simulatingModel.gameState

                animation ∷ Animation
                animation = animate
                  simulatingModel.gameState
                  newGameState

              in
                Right $
                  Simulating simulatingModel
                    { animationState = Updating { gameStep }
                    , editor = simulatingModel.editor
                        { lastScriptExecution = Just
                            { finishTime: message.executionFinishTime
                            , result: Success commands
                            , startTime: message.executionStartTime
                            }
                        }
                    , gameLogs = simulatingModel.gameLogs <> newGameLogs
                    , gameState = newGameState
                    }
                    /\ Cmd.none
                      { updateAnimation = Just { animation, gameStep } }

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


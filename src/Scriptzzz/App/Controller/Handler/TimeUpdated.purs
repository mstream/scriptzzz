module Scriptzzz.App.Controller.Handler.TimeUpdated (handle) where

import Prelude

import Data.DateTime.Instant (diff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Scriptzzz.App.Command (Commands)
import Scriptzzz.App.Controller.Handler.Types (HandleMessage)
import Scriptzzz.App.Message (Message(..), TimeUpdatedMessage)
import Scriptzzz.App.Model.EditorState (EditorState(..))
import Scriptzzz.Game as Game
import Scriptzzz.Sandbox (ExecutionResult(..))

handle ∷ HandleMessage TimeUpdatedMessage
handle model currentTime = case model.editorState of
  ExecutingScript _ →
    Right $ model /\ mempty

  Idle idleState →
    Right $ case idleState.scriptExecutionOutcome of
      Just outcome →
        case outcome.result of
          Success commands →
            let
              newGameState /\ logs = Game.update
                commands
                model.gameSettings.environment
                model.gameState
            in
              model { gameState = newGameState } /\ mempty

          _ →
            model /\ mempty

      Nothing →
        model /\ mempty

  Typing typingState →
    let
      userHasStoppedTyping = diff currentTime typingState.lastUpdateTime
        >
          Milliseconds 1000.0

      isSourceSameAsBeforeTyping = typingState.currentScript ==
        typingState.previousIdleState.script

    in
      Right
        if userHasStoppedTyping then
          if isSourceSameAsBeforeTyping then
            model { editorState = Idle typingState.previousIdleState }
              /\
                mempty
          else
            model
              { editorState = ExecutingScript
                  { executionStart: currentTime
                  , previousIdleState: typingState.previousIdleState
                  , script: typingState.currentScript
                  }
              } /\ (mempty ∷ Commands Game.Commands)
              { evaluateScript =
                  [ { handleEffectResult:
                        \{ executionFinishTime
                         , executionResult
                         , executionStartTime
                         } → Just $ ScriptEvaluated
                          { executionFinishTime
                          , executionResult
                          , executionStartTime
                          , scheduledTime: currentTime
                          }
                    , parameters: { script: typingState.currentScript }
                    }
                  ]
              }
        else model { editorState = Typing typingState } /\ mempty


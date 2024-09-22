module Scriptzzz.App.Controller.Handler.ScriptEvaluated (handle) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Scriptzzz.App.Command (Commands)
import Scriptzzz.App.Controller.Handler.Types (HandleMessage)
import Scriptzzz.App.Message (ScriptEvaluatedMessage)
import Scriptzzz.App.Model (Model)
import Scriptzzz.App.Model.EditorState (EditorState(..))
import Scriptzzz.Game as Game

handle ∷ HandleMessage ScriptEvaluatedMessage
handle
  model
  { executionFinishTime
  , executionResult
  , executionStartTime
  , scheduledTime
  } =
  case model.editorState of
    ExecutingScript executingScriptState →
      result
      where
      result ∷ String \/ Model /\ Commands Game.Commands
      result
        | scheduledTime == executingScriptState.executionStart = Right $
            model
              { editorState = Idle
                  { script: executingScriptState.script
                  , scriptExecutionOutcome: Just
                      { finish: executionFinishTime
                      , result: executionResult
                      , start: executionStartTime
                      }
                  }
              } /\ mempty
        | scheduledTime < executingScriptState.executionStart = Right $
            model /\ mempty
        | otherwise = Left "received unexpected evaluated script result"

    Idle _ →
      Right $ model /\ (mempty ∷ Commands Game.Commands)
        { logDebug =
            [ { handleEffectResult: const Nothing
              , parameters:
                  { message: "Ignoring script stale evaluation result" }
              }
            ]
        }

    Typing _ →
      Right $ model /\ (mempty ∷ Commands Game.Commands)
        { logDebug =
            [ { handleEffectResult: const Nothing
              , parameters:
                  { message: "Ignoring script stale evaluation result" }
              }
            ]
        }


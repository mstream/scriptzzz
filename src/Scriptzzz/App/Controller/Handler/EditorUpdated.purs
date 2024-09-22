module Scriptzzz.App.Controller.Handler.EditorUpdated (handle) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Scriptzzz.App.Controller.Handler.Types (HandleMessage)
import Scriptzzz.App.Message (EditorUpdatedMessage)
import Scriptzzz.App.Model.EditorState (EditorState(..))

handle ∷ HandleMessage EditorUpdatedMessage
handle model { time, value } =
  case model.editorState of
    ExecutingScript executingScriptState →
      Right $
        model
          { editorState = Typing
              { currentScript: value
              , lastUpdateTime: time
              , previousIdleState:
                  executingScriptState.previousIdleState
              }
          } /\ mempty

    Idle idleState →
      Right $
        model
          { editorState = Typing
              { currentScript: value
              , lastUpdateTime: time
              , previousIdleState: idleState
              }
          } /\ mempty

    Typing typingState →
      Right $
        model
          { editorState =
              Typing typingState
                { currentScript = value
                , lastUpdateTime = time
                }
          } /\ mempty


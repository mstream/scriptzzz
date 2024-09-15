module Scriptzzz.App where

import Prelude

import Data.DateTime.Instant (Instant, diff)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NES
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Flame (Application, Subscription)
import Scriptzzz.App.Message (Message(..))
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.App.View (view)
import Scriptzzz.Game (Entity(..))
import Scriptzzz.Game as Game
import Scriptzzz.Game.Command (Commands)
import Scriptzzz.Game.Types (Id(..))
import Scriptzzz.Sandbox (ExecutionResult(..), runProgram)
import Type.Proxy (Proxy(..))

app ∷ Application Model Message
app = { init, subscribe, update, view }

init ∷ Model /\ Array (Aff (Maybe Message))
init = model /\ commands
  where
  model ∷ Model
  model = Idle Nothing

  commands ∷ Array (Aff (Maybe Message))
  commands = []

subscribe ∷ Array (Subscription Message)
subscribe = []

update ∷ Model → Message → Model /\ Array (Aff (Maybe Message))
update model = case _ of
  EditorUpdated payload →
    handleEditorUpdatedMessage payload model
  ScriptEvaluated payload →
    handleScriptEvaluatedMessage payload model
  TimeUpdated payload →
    handleTimeUpdatedMessage payload model

handleEditorUpdatedMessage
  ∷ { time ∷ Instant, value ∷ String }
  → Model
  → Model /\ Array (Aff (Maybe Message))
handleEditorUpdatedMessage { time, value } = case _ of
  Evaluating st →
    Typing
      { currentSource: value
      , lastUpdateTime: time
      , previousIdleState: st.previousIdleState
      } /\ []

  Idle st →
    Typing
      { currentSource: value
      , lastUpdateTime: time
      , previousIdleState: st
      } /\ []

  Typing st →
    Typing st
      { currentSource = value
      , lastUpdateTime = time
      } /\ []

handleScriptEvaluatedMessage
  ∷ { evaluatedTime ∷ Instant
    , scheduledTime ∷ Instant
    , value ∷ ExecutionResult Commands
    }
  → Model
  → Model /\ Array (Aff (Maybe Message))
handleScriptEvaluatedMessage { evaluatedTime, scheduledTime, value } =
  case _ of
    Evaluating state →
      if scheduledTime >= state.scheduledTime then
        ( Idle $ Just
            { executionResult: value
            , executionTime: evaluatedTime
            , gameState: Map.singleton 
                (Id $ NES.nes (Proxy :: _ "foo"))
                (Worker {position : {x: 0, y: 0}, task: Nothing})
            , source: state.source
            }
        ) /\ []
      else Evaluating state /\ []

    Idle mbState →
      case mbState of
        Just state ->
          case state.executionResult of
            Success commands ->
              let 
                newGameState /\ logs = Game.update 
                  commands 
                  {height: 0, width: 0} 
                  state.gameState
              in (Idle $ Just $ state {gameState = newGameState}) /\ []
            _ ->
              (Idle $ Just state) /\ []
          
        Nothing -> 
          Idle Nothing /\ []
    Typing st →
      Typing st /\ []

handleTimeUpdatedMessage
  ∷ Instant → Model → Model /\ Array (Aff (Maybe Message))
handleTimeUpdatedMessage currentTime = case _ of
  Evaluating st →
    Evaluating st /\ []

  Idle st →
    Idle st /\ [ Nothing <$ (Console.log $ show currentTime) ]

  Typing st →
    let
      userHasStoppedTyping = diff currentTime st.lastUpdateTime >
        Milliseconds 1000.0

      isSourceSameAsBeforeTyping = case st.previousIdleState of
        Nothing →
          st.currentSource == ""
        Just previousIdleState →
          st.currentSource == previousIdleState.source

    in
      if userHasStoppedTyping then
        if isSourceSameAsBeforeTyping then Idle st.previousIdleState /\
          []
        else
          Evaluating
            { previousIdleState: st.previousIdleState
            , scheduledTime: currentTime
            , source: st.currentSource
            } /\
            [ do
                executionResult ← runProgram (Milliseconds 1000.0) st.currentSource
                evaluatedTime ← liftEffect now
                pure $ Just $ ScriptEvaluated
                  { evaluatedTime
                  , scheduledTime: currentTime
                  , value: executionResult
                  }
            ]
      else Typing st /\ []


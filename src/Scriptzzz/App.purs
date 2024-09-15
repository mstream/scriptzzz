module Scriptzzz.App where

import Prelude

import Control.Monad.Error.Class (try)
import Data.DateTime.Instant (Instant, diff)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Now (now)
import Flame (Application, Html, Subscription)
import Flame.Html.Element as HE
import Scriptzzz.App.Message (Message(..))
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.Editor (createEditor)
import Scriptzzz.Global (sendMessage)
import Scriptzzz.Sandbox (execJs)

app :: Application Model Message
app = { init, subscribe, update, view }

init :: Model /\ Array (Aff (Maybe Message))
init = model /\ commands
  where
  model :: Model 
  model = Idle Nothing

  commands :: Array (Aff (Maybe Message))
  commands = []

subscribe :: Array (Subscription Message)
subscribe = []

update :: Model -> Message -> Model /\ Array (Aff (Maybe Message))
update model = case _ of
    EditorUpdated payload -> 
      handleEditorUpdatedMessage payload model
    ScriptEvaluated payload ->
      handleScriptEvaluatedMessage payload model
    TimeUpdated payload -> 
      handleTimeUpdatedMessage payload model
handleEditorUpdatedMessage :: {time :: Instant, value :: String} -> Model -> Model /\ Array (Aff (Maybe Message))
handleEditorUpdatedMessage {time, value} = case _ of
        Evaluating st ->
          Typing {
              currentSource : value,
              lastUpdateTime : time, 
              previousIdleState : st.previousIdleState
          } /\ []

        Idle st ->
          Typing {
              currentSource : value, 
              lastUpdateTime : time, 
              previousIdleState: st
          } /\ []

        Typing st ->
          Typing st {
            currentSource = value, 
            lastUpdateTime = time
          } /\ []

handleScriptEvaluatedMessage ::  {evaluatedTime :: Instant, scheduledTime :: Instant, value :: Error \/ String} -> Model -> Model /\ Array (Aff (Maybe Message))
handleScriptEvaluatedMessage  {evaluatedTime, scheduledTime, value}  = case _ of
  Evaluating st ->
    if scheduledTime >= st.scheduledTime then 
        (Idle $ Just {outcome : value, outcomeTime: evaluatedTime, source: st.source}) /\ []
    else Evaluating st /\ []
  
  Idle st -> 
    Idle st /\ []

  Typing st ->
    Typing st /\ [] 


handleTimeUpdatedMessage ::  Instant -> Model -> Model /\ Array (Aff (Maybe Message))
handleTimeUpdatedMessage currentTime =      case _ of
  Evaluating st ->
    Evaluating st /\ []
  
  Idle st -> 
    Idle st /\ [Nothing <$ (Console.log $ show currentTime)]

  Typing st ->
    let 
      userHasStoppedTyping = diff currentTime st.lastUpdateTime > Milliseconds 1000.0

      isSourceSameAsBeforeTyping = case st.previousIdleState of
        Nothing ->
          st.currentSource == ""
        Just previousIdleState ->
          st.currentSource == previousIdleState.source
          
    in if userHasStoppedTyping then
      if isSourceSameAsBeforeTyping then Idle st.previousIdleState /\ []
        else Evaluating {previousIdleState: st.previousIdleState, scheduledTime: currentTime,source: st.currentSource} /\ [
          do 
            programOutputResult <- try $ execJs 1000 st.currentSource
            evaluatedTime <- liftEffect now
            pure $ Just $ ScriptEvaluated {evaluatedTime, scheduledTime: currentTime, value: programOutputResult }
        ]
      else Typing st /\ []

view :: Model -> Html Message
view model = HE.main "main" [
  HE.managed_ 
    {
      createNode: const $ createEditor \updatedContents -> do
        currentTime <- now
        sendMessage $ EditorUpdated {time: currentTime, value: updatedContents}
    , updateNode: \n _ _ -> pure n 
    } 
    unit   
  , HE.text case model of
      Evaluating _ -> 
        "evaluating..."
      Idle payload -> 
        case payload of 
          Nothing -> 
            ""
          Just {outcome} -> 
            show outcome
      Typing _ ->
        "typing..."

]

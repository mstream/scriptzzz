module Scriptzzz.App.Controller (DebugLevel(..), init, update) where

import Scriptzzz.Prelude

import Data.String as S
import Scriptzzz.App.Command
  ( CommandExecutor
  , CommandParameters
  , CommandResultHandler
  , Commands
  )
import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Controller.Handler.AnimationUpdated as AnimationUpdated
import Scriptzzz.App.Controller.Handler.CanvasInitialized as CanvasInitialized
import Scriptzzz.App.Controller.Handler.EditorUpdated as EditorUpdated
import Scriptzzz.App.Controller.Handler.ScriptExecuted as ScriptExecuted
import Scriptzzz.App.Controller.Handler.SimulationStartRequested as SimulationStartRequested
import Scriptzzz.App.Controller.Handler.SimulationStopRequested as SimulationStopRequested
import Scriptzzz.App.Controller.Handler.TimeUpdated as TimeUpdated
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))

data DebugLevel
  = Full (Model -> String)
  | MessageNamesOnly
  | MessagesOnly
  | None

init ∷ Model /\ Array (Aff (Maybe Msg.Message))
init = CanvasInitializing /\ []

update
  ∷ { | Commands (CommandExecutor Aff) }
  -> DebugLevel
  → Model
  → Msg.Message
  → Model /\ Array (Aff (Maybe Msg.Message))
update commandExecutors debugLevel previousModel message =
  let
    modelAndCommandsResult
      ∷ String \/ Model /\ { | Commands CommandParameters }
    modelAndCommandsResult = case message.body of
      Msg.AnimationUpdated payload →
        AnimationUpdated.handle previousModel payload
      Msg.CanvasInitialized payload →
        CanvasInitialized.handle previousModel payload
      Msg.EditorUpdated payload →
        EditorUpdated.handle previousModel payload
      Msg.ScriptExecuted payload →
        ScriptExecuted.handle previousModel payload
      Msg.SimulationStartRequested payload →
        SimulationStartRequested.handle previousModel payload
      Msg.SimulationStopRequested payload →
        SimulationStopRequested.handle previousModel payload
      Msg.TimeUpdated payload →
        TimeUpdated.handle previousModel payload

    nextModel /\ commands = handleErrors
      $ map addDebugCommands 
      $ modelAndCommandsResult

  in
    nextModel /\ Cmd.runCommands
      commandExecutors
      commandResultHandlers
      commands
  where
  addDebugCommands ::
    Model /\ { | Commands CommandParameters }
    → Model /\ { | Commands CommandParameters }
  addDebugCommands (nextModel /\ commands) =
    case debugLevel of
      Full showModel →
        let
          debugInfo ∷ String
          debugInfo = S.joinWith "\n"
            [ "==="
            , "Previous state:"
            , showModel previousModel
            , "---"
            , "Message:"
            , show message
            , "---"
            , "Next state:"
            , if nextModel == previousModel then
                "<NO_CHANGE>"
              else showModel nextModel
            , "==="
            ]

        in
          nextModel /\ commands { logDebug = Just debugInfo }

      MessageNamesOnly →
        let
          messageName ∷ String
          messageName = case message.body of
            Msg.AnimationUpdated _ →
              "Animation Updted"

            Msg.CanvasInitialized _ →
              "Canvas Initialized"

            Msg.EditorUpdated _ →
              "Editor Updted"

            Msg.ScriptExecuted _ →
              "Script Executed"

            Msg.SimulationStartRequested _ →
              "Simulation Start Requested"
            
            Msg.SimulationStopRequested _ →
              "Simulation Stop Requested"

            Msg.TimeUpdated _ →
              "Time Updated"

          debugInfo ∷ String
          debugInfo = "Message: " <> messageName
        in
          nextModel /\ commands
            { logDebug = Just debugInfo }

      MessagesOnly →
        let
          headerText ∷ String
          headerText = show message.header.creationTime

          bodyText ∷ String
          bodyText = show message.body

          debugInfo ∷ String
          debugInfo = "Message: " <> headerText <> " | " <> bodyText
        in
          nextModel /\ commands
            { logDebug = Just debugInfo }

      None →
        nextModel /\ commands

  handleErrors
    ∷ String \/ Model /\ { | Commands CommandParameters }
    → Model /\ { | Commands CommandParameters }
  handleErrors = case _ of
    Left errorMessage →
      let
        errorInfo ∷ String
        errorInfo = S.joinWith "\n"
          [ "!!!"
          , "Previous state:"
          , show previousModel
          , "---"
          , "Error Message:"
          , show errorMessage
          , "---"
          , "Next state:"
          , "<NO_CHANGE>"
          , "!!!"
          ]

      in
        previousModel /\ Cmd.none { logError = Just errorInfo }

    Right nextModelAndCommands →
      nextModelAndCommands

commandResultHandlers
  ∷ { | Commands (CommandResultHandler Error Msg.Message) }
commandResultHandlers =
  { executeScript: withCreationTime
      \{ commandExecutionResult, finishTime, startTime } →
        case commandExecutionResult of
          Left _ →
            Nothing

          Right result →
            Just $ Msg.ScriptExecuted
              { executionFinishTime: finishTime
              , executionResult: result
              , executionStartTime: startTime
              }
  , logDebug: const Nothing
  , logError: const Nothing
  , updateAnimation: withCreationTime
      \{ commandExecutionResult
       , commandParameters
       , finishTime
       , startTime
       } →
        case commandExecutionResult of
          Left _ →
            Nothing

          Right result →
            Just $ Msg.AnimationUpdated
              { executionFinishTime: finishTime
              , executionStartTime: startTime
              , gameStep: commandParameters.gameStep
              }

  }
  where
  withCreationTime
    ∷ ∀ err params result
    . (Cmd.CommandExecutionResult err params result → Maybe Msg.Body)
    → CommandResultHandler err Msg.Message params result
  withCreationTime toBody args = do
    body ← toBody args
    pure $ { body, header: { creationTime: Just args.finishTime } }


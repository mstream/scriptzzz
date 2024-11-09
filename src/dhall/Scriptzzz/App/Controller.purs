module Scriptzzz.App.Controller (DebugLevel(..), init, update) where

import Scriptzzz.Prelude

import Data.String as S
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
import Scriptzzz.PathFinding as PF

data DebugLevel ∷ ∀ k1 k2. k1 → k2 → Type
data DebugLevel w h
  = Full (Model w h → String)
  | MessageNamesOnly
  | MessagesOnly
  | None

init
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ PF.ObstacleMatrix w h
  → Model w h /\ Array (Aff (Maybe (Msg.Message w h)))
init obstacleMatrix = CanvasInitializing { obstacleMatrix } /\ []

update
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Cmd.CommandExecutors w h Aff
  → DebugLevel w h
  → Model w h
  → Msg.Message w h
  → Model w h /\ Array (Aff (Maybe (Msg.Message w h)))
update commandExecutors debugLevel previousModel message =
  let
    modelAndCommandsResult
      ∷ String \/ Model w h /\ Cmd.Commands w h
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
  addDebugCommands
    ∷ Model w h /\ Cmd.Commands w h
    → Model w h /\ Cmd.Commands w h
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
          nextModel /\ commands `Cmd.withLogDebug` debugInfo

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
          nextModel /\ commands `Cmd.withLogDebug` debugInfo

      MessagesOnly →
        let
          headerText ∷ String
          headerText = show message.header.creationTime

          bodyText ∷ String
          bodyText = show message.body

          debugInfo ∷ String
          debugInfo = "Message: " <> headerText <> " | " <> bodyText
        in
          nextModel /\ commands `Cmd.withLogDebug` debugInfo

      None →
        nextModel /\ commands

  handleErrors
    ∷ String \/ Model w h /\ Cmd.Commands w h
    → Model w h /\ Cmd.Commands w h
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
        previousModel /\ Cmd.none `Cmd.withLogError` errorInfo

    Right nextModelAndCommands →
      nextModelAndCommands

commandResultHandlers
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Cmd.CommandResultHandlers w h Error (Msg.Message w h)
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
    ∷ ∀ err h params result w
    . ( Cmd.CommandExecutionResult err params result
        → Maybe (Msg.Body w h)
      )
    → Cmd.CommandResultHandler err (Msg.Message w h) params result
  withCreationTime toBody args = do
    body ← toBody args
    pure $ { body, header: { creationTime: Just args.finishTime } }


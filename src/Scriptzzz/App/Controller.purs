module Scriptzzz.App.Controller (init, update) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.NonEmpty as NES
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec as Vec
import Effect.Aff (Aff)
import Scriptzzz.App.Command (Commands, runCommands)
import Scriptzzz.App.Controller.Handler.EditorUpdated as EditorUpdated
import Scriptzzz.App.Controller.Handler.ScriptEvaluated as ScriptEvaluated
import Scriptzzz.App.Controller.Handler.TimeUpdated as TimeUpdated
import Scriptzzz.App.Message (Message(..))
import Scriptzzz.App.Model (Model)
import Scriptzzz.App.Model.EditorState (EditorState(..), Script(..))
import Scriptzzz.Game (Entity(..))
import Scriptzzz.Game as Game
import Scriptzzz.Game.Types (Id(..))
import Scriptzzz.PathFinding (MapMatrix(..))
import Type.Proxy (Proxy(..))

data DebugLevel
  = Full (Message → String) (Model → String)
  | MessagesOnly (Message → String)
  | None

init ∷ Model /\ Array (Aff (Maybe Message))
init = model /\ commands
  where
  model ∷ Model
  model =
    { editorState: Idle
        { script: Script "", scriptExecutionOutcome: Nothing }
    , gameLogs: []
    , gameSettings:
        { environment:
            { mapMatrix: MapMatrix $ Vec.fill \_ → Vec.fill \_ → false }
        , restartOnScriptChange: false
        , stopOnError: false
        }
    , gameState: initialGameState
    }

  commands ∷ Array (Aff (Maybe Message))
  commands = []

update ∷ Model → Message → Model /\ Array (Aff (Maybe Message))
update previousModel message =
  let
    modelAndCommandsResult ∷ String \/ Model /\ Commands Game.Commands
    modelAndCommandsResult = case message of
      EditorUpdated payload →
        EditorUpdated.handle previousModel payload
      ScriptEvaluated payload →
        ScriptEvaluated.handle previousModel payload
      TimeUpdated payload →
        TimeUpdated.handle previousModel payload

    nextModel /\ commands = handleErrors
      $ map (addDebugCommands controllerDebugLevel)
      $ modelAndCommandsResult

  in
    nextModel /\ runCommands commands
  where
  addDebugCommands
    ∷ DebugLevel
    → Model /\ Commands Game.Commands
    → Model /\ Commands Game.Commands
  addDebugCommands debugLevel (nextModel /\ commands) =
    case debugLevel of
      Full showMessage showModel →
        nextModel /\ commands
          { logDebug = commands.logDebug <>
              [ { handleEffectResult: const Nothing
                , parameters:
                    { message: S.joinWith "\n"
                        [ "==="
                        , "Previous state:"
                        , showModel previousModel
                        , "---"
                        , "Message:"
                        , showMessage message
                        , "---"
                        , "Next state:"
                        , if nextModel == previousModel then
                            "<NO_CHANGE>"
                          else showModel nextModel
                        , "==="
                        ]

                    }
                }
              ]
          }

      MessagesOnly showMessage →
        nextModel /\ commands
          { logDebug = commands.logDebug <>
              [ { handleEffectResult: const Nothing
                , parameters:
                    { message: "Message: " <> showMessage message }
                }
              ]
          }

      None →
        nextModel /\ commands

  handleErrors
    ∷ String \/ Model /\ Commands Game.Commands
    → Model /\ Commands Game.Commands
  handleErrors = case _ of
    Left errorMessage →
      previousModel /\ (mempty ∷ Commands Game.Commands)
        { logError =
            [ { handleEffectResult: const Nothing
              , parameters:
                  { message: S.joinWith "\n"
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
                  }
              }
            ]
        }

    Right nextModelAndCommands →
      nextModelAndCommands

initialGameState ∷ Game.State
initialGameState = Game.State $ Map.singleton
  (Id $ NES.nes (Proxy ∷ _ "foo"))
  (Worker { position: { x: 15, y: 15 }, task: Nothing })

controllerDebugLevel ∷ DebugLevel
controllerDebugLevel = Full showMessage showModel
  where
  showMessage ∷ Message → String
  showMessage = case _ of
    EditorUpdated _ →
      "Editor Updted"

    ScriptEvaluated _ →
      "Script Evaluated"

    TimeUpdated _ →
      "Time Updated"

  showModel ∷ Model → String
  showModel = _.editorState >>> case _ of
    ExecutingScript _ →
      "Executing Script"

    Idle _ →
      "Idle"

    Typing _ →
      "Typing"


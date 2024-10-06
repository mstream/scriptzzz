module Scriptzzz.App.Command
  ( CommandExecutionResult
  , CommandExecutor
  , CommandParameters
  , CommandResultHandler
  , Commands
  , none
  , runCommands
  ) where

import Scriptzzz.Prelude

import Data.Array as A
import Scriptzzz.App.Model.AnimationState (GameStep)
import Scriptzzz.Canvas.Animation (Animation)
import Scriptzzz.Core (Script, Timestamp, timestamp)
import Scriptzzz.Game as Game
import Scriptzzz.Sandbox (ExecutionResult)

type Commands ∷ (Type → Type → Type) → Row Type
type Commands f =
  ( executeScript ∷ f Script (ExecutionResult Game.Commands)
  , logDebug ∷ f String Unit
  , logError ∷ f String Unit
  , updateAnimation ∷
      f { animation ∷ Animation, gameStep ∷ GameStep }
        { errors ∷ Array String }
  )

type CommandParameters ∷ Type → Type → Type
type CommandParameters params result = Maybe params

type CommandResult ∷ Type → Type → Type
type CommandResult params result = result

type CommandExecutor ∷ (Type → Type) → Type → Type → Type
type CommandExecutor m params result = params → m result

type CommandExecutionResult err params result =
  { commandExecutionResult ∷ err \/ result
  , commandParameters ∷ params
  , finishTime ∷ Timestamp
  , startTime ∷ Timestamp
  }

type CommandResultHandler ∷ Type → Type → Type → Type → Type
type CommandResultHandler err msg params result =
  CommandExecutionResult err params result → Maybe msg

none ∷ { | Commands CommandParameters }
none =
  { executeScript: Nothing
  , logDebug: Nothing
  , logError: Nothing
  , updateAnimation: Nothing
  }

runCommands
  ∷ ∀ err m msg
  . MonadEffect m
  ⇒ MonadError err m
  ⇒ { | Commands (CommandExecutor m) }
  → { | Commands (CommandResultHandler err msg) }
  → { | Commands CommandParameters }
  → Array (m (Maybe msg))
runCommands executors handlers parameters = A.catMaybes
  [ runCommand
      executors.executeScript
      handlers.executeScript
      parameters.executeScript
  , runCommand
      executors.logDebug
      handlers.logDebug
      parameters.logDebug
  , runCommand
      executors.logError
      handlers.logError
      parameters.logError
  , runCommand
      executors.updateAnimation
      handlers.updateAnimation
      parameters.updateAnimation
  ]

runCommand
  ∷ ∀ err m msg params result
  . MonadEffect m
  ⇒ MonadError err m
  ⇒ CommandExecutor m params result
  → CommandResultHandler err msg params result
  → CommandParameters params result
  → Maybe (m (Maybe msg))
runCommand execute handleResult = map \params → do
  startTime ← timestamp <$> liftEffect now
  commandExecutionResult ← try $ execute params
  finishTime ← timestamp <$> liftEffect now
  pure $ handleResult
    { commandExecutionResult
    , commandParameters: params
    , finishTime
    , startTime
    }


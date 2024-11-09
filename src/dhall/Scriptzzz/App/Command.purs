module Scriptzzz.App.Command
  ( CommandExecutionResult
  , CommandExecutor
  , CommandExecutors
  , CommandParameters
  , CommandResultHandlers
  , Commands
  , CommandResultHandler
  , CommandsRow
  , ExecuteScriptParameters
  , LogParameters
  , UpdateAnimationParameters
  , none
  , runCommands
  , withExecuteScript
  , withLogDebug
  , withLogError
  , withUpdateAnimation
  ) where

import Scriptzzz.Prelude

import Data.Array as A
import Scriptzzz.App.Model.AnimationState (GameStep)
import Scriptzzz.Canvas.Animation (Animation)
import Scriptzzz.Core (Script, Timestamp, timestamp)
import Scriptzzz.Game as Game
import Scriptzzz.Sandbox (ExecutionResult)

type CommandsRow ∷ ∀ k1 k2. k1 → k2 → (Type → Type → Type) → Row Type
type CommandsRow w h f =
  ( executeScript ∷
      f
        ExecuteScriptParameters
        (ExecutionResult (Game.Commands w h))
  , logDebug ∷ f LogParameters Unit
  , logError ∷ f LogParameters Unit
  , updateAnimation ∷
      f
        (UpdateAnimationParameters w h)
        { errors ∷ Array String }
  )

type ExecuteScriptParameters = Script

type LogParameters = String

type UpdateAnimationParameters ∷ ∀ k1 k2. k1 → k2 → Type
type UpdateAnimationParameters w h =
  { animation ∷ Animation w h, gameStep ∷ GameStep }

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

newtype Commands ∷ ∀ k1 k2. k1 → k2 → Type
newtype Commands w h = Commands { | CommandsRow w h CommandParameters }

type CommandExecutors ∷ ∀ k1 k2. k1 → k2 → (Type → Type) → Type
type CommandExecutors w h m = { | CommandsRow w h (CommandExecutor m) }

type CommandResultHandlers ∷ ∀ k1 k2. k1 → k2 → Type → Type → Type
type CommandResultHandlers w h err msg =
  { | CommandsRow w h (CommandResultHandler err msg) }

none ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Commands w h
none = Commands
  { executeScript: Nothing
  , logDebug: Nothing
  , logError: Nothing
  , updateAnimation: Nothing
  }

withExecuteScript
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Commands w h
  → ExecuteScriptParameters
  → Commands w h
withExecuteScript (Commands parameters) executeScriptParameters =
  Commands $ parameters
    { executeScript = Just executeScriptParameters }

withLogDebug
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Commands w h
  → LogParameters
  → Commands w h
withLogDebug (Commands parameters) logParameters =
  Commands $ parameters
    { logDebug = Just logParameters }

withLogError
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Commands w h
  → LogParameters
  → Commands w h
withLogError (Commands parameters) logParameters =
  Commands $ parameters
    { logError = Just logParameters }

withUpdateAnimation
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Commands w h
  → UpdateAnimationParameters w h
  → Commands w h
withUpdateAnimation (Commands parameters) updateAnimationParameters =
  Commands $ parameters
    { updateAnimation = Just updateAnimationParameters }

runCommands
  ∷ ∀ err h m msg w
  . MonadEffect m
  ⇒ MonadError err m
  ⇒ Pos h
  ⇒ Pos w
  ⇒ { | CommandsRow w h (CommandExecutor m) }
  → { | CommandsRow w h (CommandResultHandler err msg) }
  → Commands w h
  → Array (m (Maybe msg))
runCommands executors handlers (Commands parameters) = A.catMaybes
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


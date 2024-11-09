module Scriptzzz.App.Message
  ( AnimationUpdatedPayload
  , Body(..)
  , CanvasInitializedPayload
  , EditorUpdatedPayload
  , Header
  , Message(..)
  , ScriptExecutedPayload
  , SimulationStartRequestedPayload
  , SimulationStopRequestedPayload
  , TimeUpdatedPayload
  , createWithoutTimestamp
  , createWithTimestamp
  ) where

import Scriptzzz.Prelude

import Scriptzzz.App.Model.AnimationState (GameStep)
import Scriptzzz.Core (Script, Timestamp, timestamp)
import Scriptzzz.Game as Game
import Scriptzzz.Sandbox (ExecutionResult)

type Message ∷ ∀ k1 k2. k1 → k2 → Type
type Message w h =
  { body ∷ Body w h
  , header ∷ Header
  }

type Header = { creationTime ∷ Maybe Timestamp }

data Body ∷ ∀ k1 k2. k1 → k2 → Type
data Body w h
  = AnimationUpdated AnimationUpdatedPayload
  | CanvasInitialized CanvasInitializedPayload
  | EditorUpdated EditorUpdatedPayload
  | ScriptExecuted (ScriptExecutedPayload w h)
  | SimulationStartRequested SimulationStartRequestedPayload
  | SimulationStopRequested SimulationStopRequestedPayload
  | TimeUpdated TimeUpdatedPayload

derive instance Generic (Body w h) _

instance Show (Body w h) where
  show = genericShow

type AnimationUpdatedPayload =
  { executionFinishTime ∷ Timestamp
  , executionStartTime ∷ Timestamp
  , gameStep ∷ GameStep
  }

type CanvasInitializedPayload = Unit

type EditorUpdatedPayload = Script

type SimulationStartRequestedPayload = Unit

type SimulationStopRequestedPayload = Unit

type ScriptExecutedPayload ∷ ∀ k1 k2. k1 → k2 → Type
type ScriptExecutedPayload w h =
  { executionFinishTime ∷ Timestamp
  , executionResult ∷ ExecutionResult (Game.Commands w h)
  , executionStartTime ∷ Timestamp
  }

type TimeUpdatedPayload = Timestamp

createWithoutTimestamp ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Body w h → Message w h
createWithoutTimestamp body =
  { body, header: { creationTime: Nothing } }

createWithTimestamp
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Body w h → Effect (Message w h)
createWithTimestamp body = do
  creationTime ← timestamp <$> now
  pure { body, header: { creationTime: Just creationTime } }


module Scriptzzz.App.Message (Message(..), EditorUpdatedMessage, ScriptEvaluatedMessage, TimeUpdatedMessage) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Scriptzzz.App.Model.EditorState (Script)
import Scriptzzz.Game.Command (Commands)
import Scriptzzz.Sandbox (ExecutionResult)

data Message
  = EditorUpdated EditorUpdatedMessage
  | ScriptEvaluated ScriptEvaluatedMessage
  | TimeUpdated Instant

derive instance Generic Message _

instance Show Message where
  show = genericShow

type EditorUpdatedMessage = { time ∷ Instant, value ∷ Script }

type ScriptEvaluatedMessage =
  { executionFinishTime ∷ Instant
  , executionResult :: ExecutionResult Commands
  , executionStartTime :: Instant
  , scheduledTime ∷ Instant
  }

type TimeUpdatedMessage = Instant



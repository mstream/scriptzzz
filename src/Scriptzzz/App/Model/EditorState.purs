module Scriptzzz.App.Model.EditorState
  ( EditorState(..)
  , ExecutingScriptState
  , IdleState
  , TypingState
  , Script(..)
  , ScriptExecutionOutcome
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Scriptzzz.Game as Game
import Scriptzzz.JSON (writeForeignTaggedSum)
import Scriptzzz.Sandbox (ExecutionResult)
import Yoga.JSON (class WriteForeign)

data EditorState
  = ExecutingScript ExecutingScriptState
  | Idle IdleState
  | Typing TypingState

derive instance Generic EditorState _

instance WriteForeign EditorState where
  writeImpl = writeForeignTaggedSum

instance Eq EditorState where
  eq = genericEq

instance Show EditorState where
  show = genericShow

type ExecutingScriptState =
  { executionStart ∷ Instant
  , previousIdleState ∷ IdleState
  , script ∷ Script
  }

type IdleState =
  { script ∷ Script
  , scriptExecutionOutcome ∷ Maybe ScriptExecutionOutcome
  }

type TypingState =
  { currentScript ∷ Script
  , lastUpdateTime ∷ Instant
  , previousIdleState ∷ IdleState
  }

type ScriptExecutionOutcome =
  { finish ∷ Instant
  , result ∷ ExecutionResult Game.Commands
  , start ∷ Instant
  }

newtype Script = Script String

derive newtype instance Eq Script
derive newtype instance Show Script
derive newtype instance WriteForeign Script

newtype UtcTime = UtcTime Instant

derive newtype instance Eq UtcTime
derive newtype instance Show UtcTime
derive newtype instance WriteForeign UtcTime


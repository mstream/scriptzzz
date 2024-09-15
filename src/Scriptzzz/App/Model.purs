module Scriptzzz.App.Model
  ( EvaluatingState
  , IdleState
  , TypingState
  , Model(..)
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Scriptzzz.Game as Game
import Scriptzzz.Game.Command (Commands)
import Scriptzzz.Sandbox (ExecutionResult)

data Model
  = Evaluating EvaluatingState
  | Idle IdleState
  | Typing TypingState

derive instance Generic Model _

instance Show Model where
  show = genericShow

type EvaluatingState =
  { scheduledTime ∷ Instant
  , previousIdleState ∷ IdleState
  , source ∷ String
  }

type IdleState = Maybe
  { executionResult ∷ ExecutionResult Commands
  , executionTime ∷ Instant
  , gameState ∷ Game.State
  , source ∷ String
  }

type TypingState =
  { currentSource ∷ String
  , lastUpdateTime ∷ Instant
  , previousIdleState ∷ IdleState
  }


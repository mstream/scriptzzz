module Scriptzzz.App.Model (IdleState, Model(..)) where

import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe)
import Scriptzzz.Command (Commands)
import Scriptzzz.Sandbox (ExecutionResult)

data Model
  = Evaluating { scheduledTime :: Instant, previousIdleState :: IdleState, source :: String }
  | Idle IdleState
  | Typing { currentSource :: String, lastUpdateTime :: Instant, previousIdleState :: IdleState }

type IdleState = Maybe { executionResult :: ExecutionResult Commands, executionTime :: Instant, source :: String }


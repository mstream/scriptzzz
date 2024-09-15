module Scriptzzz.App.Model (IdleState, Model(..)) where

import Data.DateTime.Instant (Instant)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe)
import Effect.Exception (Error)

data Model
  = Evaluating { scheduledTime :: Instant, previousIdleState :: IdleState, source :: String }
  | Idle IdleState
  | Typing { currentSource :: String, lastUpdateTime :: Instant, previousIdleState :: IdleState }

type IdleState = Maybe { outcome :: Error \/ String, outcomeTime :: Instant, source :: String }


module Scriptzzz.App.Message (Message(..)) where

import Data.Either.Nested (type (\/))
import Data.DateTime.Instant (Instant)
import Effect.Exception (Error)

data Message = 
  EditorUpdated {time :: Instant, value :: String}
  | ScriptEvaluated {evaluatedTime :: Instant, scheduledTime :: Instant, value :: Error \/ String} 
  | TimeUpdated Instant





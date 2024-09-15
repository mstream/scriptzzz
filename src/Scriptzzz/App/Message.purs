module Scriptzzz.App.Message (Message(..)) where

import Data.DateTime.Instant (Instant)
import Scriptzzz.Command (Commands)
import Scriptzzz.Sandbox (ExecutionResult)

data Message = 
  EditorUpdated {time :: Instant, value :: String}
  | ScriptEvaluated {evaluatedTime :: Instant, scheduledTime :: Instant, value :: ExecutionResult Commands} 
  | TimeUpdated Instant





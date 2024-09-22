module Scriptzzz.App.Command
  ( Command
  , CommandEffect
  , Commands
  , EvaluateScriptParameters
  , EvaluateScriptEffectResult
  , LogParameters
  , runCommands
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Scriptzzz.App.Message (Message)
import Scriptzzz.App.Model.EditorState (Script(..))
import Scriptzzz.Sandbox (ExecutionResult, runProgram)
import Yoga.JSON (class ReadForeign)

type Commands a =
  { evaluateScript ∷
      Array
        ( Command EvaluateScriptParameters
            (EvaluateScriptEffectResult a)
        )
  , logDebug ∷ Array (Command LogParameters Unit)
  , logError ∷ Array (Command LogParameters Unit)
  }

type Command r a =
  { handleEffectResult ∷ a → Maybe Message
  , parameters ∷ Record r
  }

type RunCommand r a = Record r → Aff a

type EvaluateScriptParameters = (script ∷ Script)

type EvaluateScriptEffectResult a =
  { executionFinishTime ∷ Instant
  , executionResult ∷ ExecutionResult a
  , executionStartTime ∷ Instant
  }

type LogParameters = (message ∷ String)

type CommandEffect = Aff (Maybe Message)

runCommands ∷ ∀ a. ReadForeign a ⇒ Commands a → Array CommandEffect
runCommands { evaluateScript, logDebug, logError } =
  ( evaluateScript <#> \{ handleEffectResult, parameters } →
      handleEffectResult <$> runEvaluateScript parameters
  )
    <>
      ( logDebug <#> \{ handleEffectResult, parameters } →
          handleEffectResult <$> runLogDebug parameters
      )
    <>
      ( logError <#> \{ handleEffectResult, parameters } →
          handleEffectResult <$> runLogError parameters
      )

runEvaluateScript
  ∷ ∀ a
  . ReadForeign a
  ⇒ RunCommand EvaluateScriptParameters (EvaluateScriptEffectResult a)
runEvaluateScript { script: (Script scriptCode) } = do
  executionStartTime ← liftEffect now
  executionResult ← runProgram (Milliseconds 1000.0) scriptCode
  executionFinishTime ← liftEffect now
  pure
    { executionFinishTime
    , executionResult
    , executionStartTime
    }

runLogDebug ∷ Record LogParameters → Aff Unit
runLogDebug { message } = Console.debug message

runLogError ∷ Record LogParameters → Aff Unit
runLogError { message } = Console.error message


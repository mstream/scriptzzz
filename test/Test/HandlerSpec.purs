module Test.HandlerSpec
  ( ExpectedCommand
  , ExpectedCommands
  , ScenarioConfiguration
  , TimeMock
  , handlerSpec
  ) where

import Prelude

import Control.Monad.Except (throwError)
import Data.Array as A
import Data.DateTime (DateTime, adjust)
import Data.DateTime.Instant (Instant, fromDateTime)
import Data.Either (Either(..), either, note)
import Data.Either.Nested (type (\/))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Scriptzzz.App.Command
  ( Command
  , Commands
  , EvaluateScriptEffectResult
  , EvaluateScriptParameters
  , LogParameters
  )
import Scriptzzz.App.Controller.Handler.Types (HandleMessage)
import Scriptzzz.App.Message (Message)
import Scriptzzz.App.Model (Model)
import Scriptzzz.Game as Game
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail)
import Test.Utils (checkEqual)
import Yoga.JSON as JSON

type ScenarioConfiguration msg =
  { expectedCommands ∷ ExpectedCommands
  , expectedModelResult ∷ String \/ Model
  , message ∷ msg
  , state ∷ Model
  }

type ExpectedCommands =
  { evaluateScript ∷
      Array
        ( ExpectedCommand EvaluateScriptParameters
            (EvaluateScriptEffectResult Game.Commands)
        )
  , logDebug ∷ Array (ExpectedCommand LogParameters Unit)
  , logError ∷ Array (ExpectedCommand LogParameters Unit)
  }

type ExpectedCommand r a =
  { effectResult ∷ a
  , message ∷ Maybe Message
  , parameters ∷ Record r
  }

type CommandEffectResults =
  { evaluateScripts ∷ Array (EvaluateScriptEffectResult Game.Commands)
  , logDebug ∷ Array Unit
  , logError ∷ Array Unit
  }

type TimeMock =
  { farFuture ∷ Instant
  , farPast ∷ Instant
  , future ∷ Instant
  , now ∷ Instant
  , past ∷ Instant
  }

handlerSpec
  ∷ ∀ msg
  . String
  → HandleMessage msg
  → String
  → (TimeMock → ScenarioConfiguration msg)
  → Spec Unit
handlerSpec titlePrefix handleMessage titleSuffix makeConf =
  it (titlePrefix <> ": " <> titleSuffix) do
    timeMock ← mockTime
    let
      { expectedCommands, expectedModelResult, message, state } =
        makeConf timeMock
    case handleMessage state message of
      Left actualErrorMessage →
        case expectedModelResult of
          Left expectedErrorMessage →
            expectedErrorMessage `checkEqual "error message"`
              actualErrorMessage

          Right _ →
            fail $ "Unexpected error: " <> actualErrorMessage

      Right (actualModel /\ actualCommands) →
        case expectedModelResult of
          Left expectedErrorMessage →
            fail $ "Missing expected error: " <> expectedErrorMessage

          Right expectedModel → do
            verifyModel { actual: actualModel, expected: expectedModel }
            verifyCommands
              { actual: actualCommands, expected: expectedCommands }

mockTime ∷ Aff TimeMock
mockTime = either (throwError <<< error) pure $ note
  "time mocking error"
  do
    let
      farPast ∷ DateTime
      farPast = bottom

    past ← adjust (Milliseconds 5000.0) farPast
    now ← adjust (Milliseconds 1.0) past
    future ← adjust (Milliseconds 1.0) now
    farFuture ← adjust (Milliseconds 5000.0) now

    Just
      { farFuture: fromDateTime farFuture
      , farPast: fromDateTime farPast
      , future: fromDateTime future
      , now: fromDateTime now
      , past: fromDateTime past
      }

verifyModel ∷ { actual ∷ Model, expected ∷ Model } → Aff Unit
verifyModel { actual, expected } = do
  JSON.writeJSON actual.editorState `checkEqual "editor state"`
    JSON.writeJSON expected.editorState
  JSON.writeJSON actual.gameLogs `checkEqual "game logs"` JSON.writeJSON
    expected.gameLogs
  JSON.writeJSON actual.gameSettings `checkEqual "game settings"`
    JSON.writeJSON expected.gameSettings
  JSON.writeJSON actual.gameState `checkEqual "game state"`
    JSON.writeJSON expected.gameState

verifyCommands
  ∷ { actual ∷ Commands Game.Commands, expected ∷ ExpectedCommands }
  → Aff Unit
verifyCommands { actual, expected } = do
  A.length actual.evaluateScript
    `checkEqual "number of EvaluateScript commands"` A.length
      expected.evaluateScript
  A.length actual.logDebug `checkEqual "number of LogDebug commands"`
    A.length expected.logDebug
  A.length actual.logError `checkEqual "number of LogError"` A.length
    expected.logError

  traverse_ (\(act /\ exp) → verifyCommand act exp)
    (A.zip actual.evaluateScript expected.evaluateScript)
  traverse_ (\(act /\ exp) → verifyCommand act exp)
    (A.zip actual.logDebug expected.logDebug)
  traverse_ (\(act /\ exp) → verifyCommand act exp)
    (A.zip actual.logError expected.logError)

verifyCommand
  ∷ ∀ a r
  . Command r a
  → ExpectedCommand r a
  → Aff Unit
verifyCommand actual expected = do
  actual.parameters `checkEqual "command parameters"`
    expected.parameters

  let
    actualMessage ∷ Maybe Message
    actualMessage = actual.handleEffectResult expected.effectResult

  actualMessage `checkEqual "command message"` expected.message


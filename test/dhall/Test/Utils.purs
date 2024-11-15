module Test.Utils (checkEqual, detailedErrorConsoleReporter) where

import Prelude

import Data.String.NonEmpty as NES
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer (class MonadWriter)
import Data.Array (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base
  ( RunningItem(..)
  , defaultReporter
  , defaultUpdate
  )
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary
import Test.Spec.Tree (Path, Tree, parentSuiteName)

type State =
  { runningItems ∷ Map Path RunningItem
  , lastPrintedSuitePath ∷ Maybe Path
  }

foreign import checkEqualImpl ∷ ∀ a. String → a → a → Effect Unit

foreign import showErrorImpl ∷ Error → String

checkEqual
  ∷ ∀ a m
  . MonadEffect m
  ⇒ MonadThrow Error m
  ⇒ NES.NonEmptyString
  → a
  → a
  → m Unit
checkEqual description actual expected = liftEffect $ checkEqualImpl
  (NES.toString description)
  actual
  expected

detailedErrorConsoleReporter ∷ Reporter
detailedErrorConsoleReporter = defaultReporter initialState $
  defaultUpdate
    { getRunningItems: _.runningItems
    , putRunningItems: flip _ { runningItems = _ }
    , printFinishedItem: \path → case _ of
        RunningTest name (Just res) → print path $ PrintTest name res
        RunningPending name → print path $ PrintPending name
        _ → pure unit
    , update: case _ of
        Event.TestEnd path name res → do
          { runningItems } ← get
          when (isNothing $ Map.lookup path runningItems) do
            print path $ PrintTest name res
        Event.Pending path name → do
          { runningItems } ← get
          when (Map.isEmpty runningItems) do
            print path $ PrintPending name
        Event.End results → printSummary results
        _ → pure unit
    }

initialState ∷ State
initialState =
  { runningItems: Map.empty, lastPrintedSuitePath: Nothing }

printSummary
  ∷ ∀ n m. MonadWriter String m ⇒ Array (Tree n Void Result) → m Unit
printSummary = Summary.summarize >>>
  \(Count { passed, failed, pending }) → do
    tellLn ""
    tellLn $ styled Style.bold "Summary"
    printPassedFailed passed failed
    printPending pending
    tellLn ""
  where
  printPassedFailed ∷ Int → Int → m Unit
  printPassedFailed p f = do
    let
      total = p + f
      testStr = pluralize "test" total
      amount = show p <> "/" <> (show total) <> " " <> testStr <>
        " passed"
      color = if f > 0 then Style.red else Style.dim
    tellLn $ styled color amount

  printPending ∷ Int → m Unit
  printPending p
    | p > 0 = tellLn $ styled Style.yellow $ show p <> " "
        <> pluralize "test" p
        <> " pending"
    | otherwise = pure unit

  pluralize ∷ String → Int → String
  pluralize s 1 = s
  pluralize s _ = s <> "s"

data PrintAction
  = PrintTest String Result
  | PrintPending String

derive instance printActionGeneric ∷ Generic PrintAction _

instance printActionShow ∷ Show PrintAction where
  show = genericShow

print
  ∷ ∀ s m
  . MonadState { lastPrintedSuitePath ∷ Maybe Path | s } m
  ⇒ MonadWriter String m
  ⇒ Path
  → PrintAction
  → m Unit
print path a = do
  s ← get
  case s.lastPrintedSuitePath of
    Just p | p == path → pure unit
    _ → do
      tellLn $ styled (Style.bold <> Style.magenta)
        $ intercalate " » " (parentSuiteName path)
      put s { lastPrintedSuitePath = Just path }
  case a of
    PrintTest name (Success _ _) → do
      tellLn $ "  " <> styled Style.green "✓︎ " <> styled Style.dim name
    PrintTest name (Failure err) → do
      tellLn $ "  " <> styled Style.red ("✗ " <> name <> ":")
      tellLn $ ""
      tellLn $ "  " <> styled Style.red (showError err)
      tellLn ""
    PrintPending name → do
      tellLn $ "  " <> styled Style.cyan ("~ " <> name)
  tellLn ""

showError ∷ Error → String
showError = showErrorImpl


module Test.Flame.Update.Handler.Scriptzzz
  ( Expectations
  , HandlerFailureScenarioConfig
  , HandlerSuccessScenarioConfig
  , ModelAssertionConfig
  , ModelAssertionM
  , RunFailureScenario
  , RunSuccessScenario
  ) where

import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Model (Model)
import Test.Flame.Update.Handler as Handler

type ModelAssertionConfig ∷ ∀ k1 k2. k1 → k2 → Type
type ModelAssertionConfig w h = Handler.ModelAssertionConfig (Model w h)

type ModelAssertionM ∷ ∀ k1 k2. k1 → k2 → Type → Type
type ModelAssertionM w h = Handler.ModelAssertionM (Model w h)

type Expectations ∷ ∀ k1 k2. k1 → k2 → Type
type Expectations w h =
  Handler.Expectations (Model w h) (Cmd.Commands w h)

type HandlerFailureScenarioConfig ∷ ∀ k1 k2. k1 → k2 → Type → Type
type HandlerFailureScenarioConfig w h payload =
  Handler.HandlerFailureScenarioConfig (Model w h) payload

type HandlerSuccessScenarioConfig ∷ ∀ k1 k2. k1 → k2 → Type → Type
type HandlerSuccessScenarioConfig w h payload =
  Handler.HandlerSuccessScenarioConfig
    (Model w h)
    (Cmd.Commands w h)
    payload

type RunFailureScenario ∷ ∀ k1 k2. k1 → k2 → Type → Type
type RunFailureScenario w h payload = Handler.RunFailureScenario
  (Model w h)
  (Cmd.Commands w h)
  payload

type RunSuccessScenario ∷ ∀ k1 k2. k1 → k2 → Type → Type
type RunSuccessScenario w h payload = Handler.RunSuccessScenario
  (Model w h)
  (Cmd.Commands w h)
  payload


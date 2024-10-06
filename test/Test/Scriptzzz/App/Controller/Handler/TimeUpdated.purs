module Test.Scriptzzz.App.Controller.Handler.TimeUpdated (spec) where

import Scriptzzz.Prelude

import Scriptzzz.App.Controller.Handler.TimeUpdated as TimeUpdated
import Scriptzzz.App.Message (TimeUpdatedPayload)
import Test.Flame.Update.Handler
  ( ConfigM
  , runFailureScenario
  , runSuccessScenario
  )
import Test.Flame.Update.Handler.Scriptzzz (HandlerFailureScenarioConfig, HandlerSuccessScenarioConfig)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.TimeUpdated" do
    pure unit 

runTimeUpdatedFailureScenario
  ∷ Int
  → ConfigM
      (Gen (HandlerFailureScenarioConfig TimeUpdatedPayload))
  → Aff Unit
runTimeUpdatedFailureScenario =
  runFailureScenario TimeUpdated.handle

runTimeUpdatedSuccessScenario
  ∷ Int
  → ConfigM
      (Gen (HandlerSuccessScenarioConfig TimeUpdatedPayload))
  → Aff Unit
runTimeUpdatedSuccessScenario =
  runSuccessScenario TimeUpdated.handle


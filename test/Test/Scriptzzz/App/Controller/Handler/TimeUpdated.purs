module Test.Scriptzzz.App.Controller.Handler.TimeUpdated (spec) where

import Scriptzzz.Prelude

import Scriptzzz.App.Controller.Handler.TimeUpdated as TimeUpdated
import Scriptzzz.App.Message (TimeUpdatedPayload)
import Test.Flame.Update.Handler as TH
import Test.Flame.Update.Handler.Scriptzzz
  ( RunFailureScenario
  , RunSuccessScenario
  )
import Test.Spec (Spec, describe)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.TimeUpdated" do
    pure unit

runFailureScenario
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ RunFailureScenario w h TimeUpdatedPayload
runFailureScenario = TH.makeRunFailureScenario TimeUpdated.handle

runSuccessScenario
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ RunSuccessScenario w h TimeUpdatedPayload
runSuccessScenario = TH.makeRunSuccessScenario TimeUpdated.handle


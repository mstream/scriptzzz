module Test.Flame.Update.Handler.Scriptzzz
  ( Expectations
  , HandlerFailureScenarioConfig
  , HandlerSuccessScenarioConfig
  , ModelAssertionConfig
  , ModelAssertionM
  ) where

import Scriptzzz.App.Command (CommandParameters, Commands)
import Scriptzzz.App.Model (Model)
import Test.Flame.Update.Handler as Handler

type ModelAssertionConfig = Handler.ModelAssertionConfig Model

type ModelAssertionM = Handler.ModelAssertionM Model

type Expectations =
  Handler.Expectations Model { | Commands CommandParameters }

type HandlerFailureScenarioConfig payload =
    Handler.HandlerFailureScenarioConfig 
    Model
    payload

type HandlerSuccessScenarioConfig payload =
    Handler.HandlerSuccessScenarioConfig 
    Model
    { | Commands CommandParameters }
    payload

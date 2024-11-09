module Scriptzzz.App.Model (EditingModel, Model(..), SimulatingModel) where

import Scriptzzz.Prelude

import Scriptzzz.App.Model.AnimationState (AnimationState)
import Scriptzzz.App.Model.GameSettings (GameSettings)
import Scriptzzz.Core (Script, Timestamp)
import Scriptzzz.Game as Game
import Scriptzzz.JSON (writeForeignTaggedSum)
import Scriptzzz.Sandbox (ExecutionResult)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Yoga.JSON (class WriteForeign)

data Model :: forall k1 k2. k1 -> k2 -> Type
data Model w h
  = CanvasInitializing (Game.Environment w h)
  | Editing (EditingModel w h)
  | Simulating (SimulatingModel w h)

derive instance Generic (Model w h) _

instance (Pos h, Pos w) => Arbitrary (Model w h) where
  arbitrary = genericArbitrary

instance (Pos h, Pos w) => WriteForeign (Model w h) where
  writeImpl = writeForeignTaggedSum

instance (Pos h, Pos w) => Eq (Model w h) where
  eq = genericEq

instance (Pos h, Pos w) => Show (Model w h) where
  show = genericShow

type EditingModel :: forall k1 k2. k1 -> k2 -> Type
type EditingModel w h =
  { gameSettings ∷ GameSettings w h
  , lastScriptExecution ∷
      Maybe
        { finishTime ∷ Timestamp
        , result ∷ ExecutionResult (Game.Commands w h)
        , startTime ∷ Timestamp
        }
  , script ∷ Script
  }

type SimulatingModel :: forall k1 k2. k1 -> k2 -> Type
type SimulatingModel w h =
  { animationState ∷ AnimationState
  , editor ∷ EditingModel w h
  , gameLogs ∷ Game.Logs
  , gameState ∷ Game.State w h
  }


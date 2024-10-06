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

data Model
  = CanvasInitializing
  | Editing EditingModel
  | Simulating SimulatingModel

derive instance Generic Model _

instance Arbitrary Model where
  arbitrary = genericArbitrary

instance WriteForeign Model where
  writeImpl = writeForeignTaggedSum

instance Eq Model where
  eq = genericEq

instance Show Model where
  show = genericShow

type EditingModel =
  { gameSettings ∷ GameSettings
  , lastScriptExecution ∷
      Maybe
        { finishTime ∷ Timestamp
        , result ∷ ExecutionResult Game.Commands
        , startTime ∷ Timestamp
        }
  , script ∷ Script
  }

type SimulatingModel =
  { animationState ∷ AnimationState
  , editor ∷ EditingModel
  , gameLogs ∷ Game.Logs
  , gameState ∷ Game.State
  }


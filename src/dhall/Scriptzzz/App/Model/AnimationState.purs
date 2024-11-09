module Scriptzzz.App.Model.AnimationState
  ( AnimationState(..)
  , GameStep
  , initialGameStep
  , nextGameStep
  ) where

import Prelude

import Control.Monad.Gen (chooseInt)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Natural (Natural, intToNat, natToInt)
import Data.Show.Generic (genericShow)
import Scriptzzz.JSON (writeForeignTaggedSum)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Yoga.JSON (class WriteForeign, writeImpl)

data AnimationState
  = Uninitialized
  | Updated { gameStep ∷ GameStep }
  | Updating { gameStep ∷ GameStep }

derive instance Generic AnimationState _

instance Arbitrary AnimationState where
  arbitrary = genericArbitrary

instance Eq AnimationState where
  eq = genericEq

instance Show AnimationState where
  show = genericShow

instance WriteForeign AnimationState where
  writeImpl = writeForeignTaggedSum

newtype GameStep = GameStep Natural

derive newtype instance Eq GameStep
derive newtype instance Show GameStep

instance Arbitrary GameStep where
  arbitrary = GameStep <<< intToNat <$> chooseInt zero top

instance WriteForeign GameStep where
  writeImpl (GameStep n) = writeImpl $ natToInt n

initialGameStep ∷ GameStep
initialGameStep = GameStep zero

nextGameStep ∷ GameStep → GameStep
nextGameStep (GameStep n) = GameStep $ n + one

module Scriptzzz.Canvas.Animation
  ( Animation
  , EntityCreation
  , EntityDestruction
  , EntityMovement
  , FPS
  , animate
  , tenFramesPerSecond
  ) where

import Scriptzzz.Prelude

import Data.Int as I
import Data.Map as M
import Data.Natural as N
import Data.Time.Duration (class Duration, Milliseconds(..), toDuration)
import Scriptzzz.Core (Id, Position)
import Scriptzzz.Game as Game
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)
import Yoga.JSON (class WriteForeign, writeImpl)

type Animation =
  { createEntity ∷ Array EntityCreation
  , destroyEntity ∷ Array EntityDestruction
  , updateEntity ∷ Array EntityMovement
  }

type EntityCreation =
  { entityType ∷ String, id ∷ Id, position ∷ Position }

type EntityDestruction =
  { id ∷ Id }

type EntityMovement =
  { id ∷ Id, sourcePosition ∷ Position, targetPosition ∷ Position }

animate ∷ Game.State → Game.State → Animation
animate (Game.State previousState) (Game.State nextState) =
  { createEntity: M.difference nextState previousState #
      foldlWithIndex create []
  , destroyEntity: M.difference previousState nextState #
      foldlWithIndex destroy []
  , updateEntity: foldlWithIndex update [] previousState
  }
  where
  create
    ∷ Id → Array EntityCreation → Game.Entity → Array EntityCreation
  create id acc = case _ of
    Game.EnergySource { position } →
      acc <> [ { id, position, entityType: "energy-source" } ]
    Game.Trail { position } →
      acc <> [ { id, position, entityType: "trail" } ]
    Game.Worker { position } →
      acc <> [ { id, position, entityType: "worker" } ]

  destroy
    ∷ Id
    → Array EntityDestruction
    → Game.Entity
    → Array EntityDestruction
  destroy id acc _ = acc <> [ { id } ]

  update
    ∷ Id
    → Array EntityMovement
    → Game.Entity
    → Array EntityMovement
  update id acc previousEntity = case M.lookup id nextState of
    Just nextEntity →
      case previousEntity of
        Game.EnergySource _ →
          acc

        Game.Trail _ →
          acc

        Game.Worker previousWorker →
          case nextEntity of
            Game.Worker nextWorker →
              if nextWorker.position /= previousWorker.position then
                acc <>
                  [ { id
                    , sourcePosition: previousWorker.position
                    , targetPosition: nextWorker.position
                    }
                  ]
              else acc

            _ →
              acc
    Nothing →
      acc

newtype FPS = FPS N.Natural

derive newtype instance Eq FPS
derive newtype instance Show FPS

instance Arbitrary FPS where
  arbitrary ∷ Gen FPS
  arbitrary = (FPS <<< N.intToNat) <$> chooseInt 1 99

instance WriteForeign FPS
  where
  writeImpl ∷ FPS → Foreign
  writeImpl (FPS n) = writeImpl $ N.natToInt n

tenFramesPerSecond ∷ FPS
tenFramesPerSecond = FPS $ N.intToNat 10

frameDuration ∷ ∀ d. Duration d ⇒ FPS → d
frameDuration (FPS n) =
  toDuration $ Milliseconds $ 1000.0 / (I.toNumber $ N.natToInt n)

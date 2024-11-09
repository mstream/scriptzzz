module Scriptzzz.PathFinding
  ( module Map
  , Path
  , buildPath
  , findPath
  , followPath
  ) where

import Scriptzzz.Prelude

import Data.Array as A
import Data.List as L
import Data.List.NonEmpty as NEL
import Foreign (MultipleErrors)
import Scriptzzz.Core (Direction, Position, move)
import Scriptzzz.PathFinding.Map
  ( ObstacleMatrix
  , emptyObstacleMatrix
  , obstacleMatrixToArray
  ) as Map
import Yoga.JSON (class ReadForeign)
import Yoga.JSON as JSON

newtype Path ∷ ∀ k1 k2. k1 → k2 → Type
newtype Path w h = Path (List (Position w h))

derive newtype instance Eq (Path w h)
derive newtype instance Show (Path w h)

instance (Pos h, Pos w) ⇒ Arbitrary (Path w h) where
  arbitrary = do
    initialPosition ← arbitrary
    directions ← arbitrary
    pure $ buildPath initialPosition directions

instance (Pos h, Pos w) ⇒ ReadForeign (Path w h) where
  readImpl f = do
    positions ∷ Array (Position w h) ← JSON.read' f
    pure $ Path $ L.fromFoldable positions

instance JSON.WriteForeign (Path w h) where
  writeImpl (Path positions) = JSON.write $ A.fromFoldable positions

foreign import findPathImpl
  ∷ ∀ h w. Array (Array Int) → Position w h → Position w h → Foreign

buildPath
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Position w h → Array Direction → Path w h
buildPath initialPosition = Path
  <<< NEL.toList
  <<< NEL.reverse
  <<< foldl reduceDirections (NEL.singleton initialPosition)
  where
  reduceDirections
    ∷ NonEmptyList (Position w h)
    → Direction
    → NonEmptyList (Position w h)
  reduceDirections acc direction =
    let
      { head, tail } = NEL.uncons acc

    in
      NEL.cons' (move head direction) (head : tail)

followPath ∷ ∀ h w. Path w h → Maybe (Position w h /\ Path w h)
followPath (Path positions) = case positions of
  Nil →
    Nothing

  nextPosition : otherPositions →
    Just $ nextPosition /\ Path otherPositions

findPath
  ∷ ∀ w h
  . Pos w
  ⇒ Pos h
  ⇒ Map.ObstacleMatrix w h
  → Position w h
  → Position w h
  → MultipleErrors \/ Path w h
findPath obstacleMatrix sourcePosition targetPosition =
  runExcept $ JSON.read' $ findPathImpl
    (convertObstacleMatrix obstacleMatrix)
    sourcePosition
    targetPosition
  where
  convertObstacleMatrix ∷ Map.ObstacleMatrix w h → Array (Array Int)
  convertObstacleMatrix = map (map if _ then 1 else 0) <<<
    Map.obstacleMatrixToArray


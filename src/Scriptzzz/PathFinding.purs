module Scriptzzz.PathFinding
  ( module Map
  , Direction(..)
  , Path
  , buildPath
  , findPath
  , followPath
  ) where

import Scriptzzz.Prelude

import Data.Array as A
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Typelevel.Num (class Nat)
import Foreign (F, ForeignError(..), MultipleErrors, fail)
import Foreign as F
import Scriptzzz.Core (Position)
import Scriptzzz.PathFinding.Map
  ( ObstacleMatrix
  , emptyObstacleMatrix
  , obstacleMatrixToArray
  ) as Map
import Yoga.JSON as JSON

data Direction
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW

derive instance Generic Direction _

instance Arbitrary Direction where
  arbitrary = genericArbitrary

newtype Path = Path (List Position)

derive newtype instance Eq Path
derive newtype instance Show Path

instance Arbitrary Path where
  arbitrary = do
    initialPosition ← arbitrary
    directions ← arbitrary
    pure $ buildPath initialPosition directions

instance JSON.WriteForeign Path where
  writeImpl (Path positions) = JSON.write $ A.fromFoldable positions

foreign import findPathImpl
  ∷ Array (Array Int) → Position → Position → Foreign

buildPath ∷ Position → Array Direction → Path
buildPath initialPosition = Path
  <<< NEL.toList
  <<< NEL.reverse
  <<< foldl reduceDirections (NEL.singleton initialPosition)
  where
  reduceDirections
    ∷ NonEmptyList Position → Direction → NonEmptyList Position
  reduceDirections acc direction =
    let
      { head, tail } = NEL.uncons acc

    in
      case direction of
        N →
          NEL.cons' head { y = head.y - 1 } (head : tail)

        NE →
          NEL.cons' { x: head.x + 1, y: head.y - 1 } (head : tail)

        E →
          NEL.cons' head { x = head.x + 1 } (head : tail)

        SE →
          NEL.cons' { x: head.x + 1, y: head.y + 1 } (head : tail)

        S →
          NEL.cons' head { y = head.y + 1 } (head : tail)

        SW →
          NEL.cons' { x: head.x - 1, y: head.y + 1 } (head : tail)

        W →
          NEL.cons' head { x = head.x - 1 } (head : tail)

        NW →
          NEL.cons' { x: head.x - 1, y: head.y - 1 } (head : tail)

followPath ∷ Path → Maybe (Position /\ Path)
followPath (Path positions) = case positions of
  Nil →
    Nothing

  nextPosition : otherPositions →
    Just $ nextPosition /\ Path otherPositions

findPath
  ∷ ∀ x y
  . Nat x
  ⇒ Nat y
  ⇒ Map.ObstacleMatrix x y
  → Position
  → Position
  → MultipleErrors \/ Path
findPath obstacleMatrix sourcePosition targetPosition =
  map (Path <<< L.fromFoldable)
    $ runExcept
    $ parseSteps
    $ findPathImpl
        (convertObstacleMatrix obstacleMatrix)
        sourcePosition
        targetPosition
  where
  convertObstacleMatrix ∷ Map.ObstacleMatrix x y → Array (Array Int)
  convertObstacleMatrix = map (map if _ then 1 else 0) <<<
    Map.obstacleMatrixToArray

  parseSteps ∷ Foreign → F (Array Position)
  parseSteps value = do
    stepTuples ← F.readArray value
    traverse (\tuple → F.readArray tuple >>= parsePosition) stepTuples
    where
    parsePosition ∷ Array Foreign → F Position
    parsePosition = case _ of
      [ xValue, yValue ] → do
        x ← F.readInt xValue
        y ← F.readInt yValue
        pure { x, y }
      xs →
        fail $ ForeignError
          $ "Step tuple has a length different from 2: "
          <> show (A.length xs)


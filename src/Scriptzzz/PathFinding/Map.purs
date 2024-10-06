module Scriptzzz.PathFinding.Map
  ( ObstacleMatrix
  , emptyObstacleMatrix
  , obstacleMatrixToArray
  ) where

import Scriptzzz.Prelude

import Data.String as S
import Data.Typelevel.Num (class Nat)
import Data.Vec as V
import Data.Vec as Vec
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Yoga.JSON (class WriteForeign, writeImpl)

newtype ObstacleMatrix ∷ ∀ k1 k2. k1 → k2 → Type
newtype ObstacleMatrix x y = ObstacleMatrix (Vec x (Vec y Boolean))

derive newtype instance (Nat x, Nat y) ⇒ Eq (ObstacleMatrix x y)

instance (Nat x, Nat y) ⇒ Arbitrary (ObstacleMatrix x y) where
  arbitrary ∷ Gen (ObstacleMatrix x y)
  arbitrary = pure $ ObstacleMatrix $ Vec.fill \_ → Vec.fill \_ → false

instance (Nat x, Nat y) ⇒ Show (ObstacleMatrix x y) where
  show = capLength <<< showFull
    where
    capLength :: String -> String
    capLength s = if S.length s < 21 then s else S.take 18 s <> "..."

    showFull :: ObstacleMatrix x y -> String
    showFull = S.joinWith "\n" <<< map renderRow <<< obstacleMatrixToArray

    renderRow ∷ Array Boolean → String
    renderRow = S.joinWith "" <<< map (if _ then "[#]" else "[ ]")

instance (Nat x, Nat y) ⇒ WriteForeign (ObstacleMatrix x y) where
  writeImpl ∷ ObstacleMatrix x y → Foreign
  writeImpl = writeImpl <<< obstacleMatrixToArray

emptyObstacleMatrix ∷ ∀ x y. Nat x ⇒ Nat y ⇒ ObstacleMatrix x y
emptyObstacleMatrix = ObstacleMatrix $ Vec.fill \_ → Vec.fill \_ → false

obstacleMatrixToArray
  ∷ ∀ x y. Nat x ⇒ Nat y ⇒ ObstacleMatrix x y → Array (Array Boolean)
obstacleMatrixToArray (ObstacleMatrix rows) =
  V.toArray <$> V.toArray rows


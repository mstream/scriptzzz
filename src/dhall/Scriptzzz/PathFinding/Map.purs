module Scriptzzz.PathFinding.Map
  ( ObstacleMatrix
  , emptyObstacleMatrix
  , obstacleMatrixToArray
  ) where

import Scriptzzz.Prelude

import Data.String as S
import Data.Typelevel.Num (class Pos)
import Data.Vec as V
import Data.Vec as Vec
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Yoga.JSON (class WriteForeign, writeImpl)

newtype ObstacleMatrix ∷ ∀ k1 k2. k1 → k2 → Type
newtype ObstacleMatrix w h = ObstacleMatrix (Vec h (Vec w Boolean))

derive newtype instance (Pos h, Pos w) ⇒ Eq (ObstacleMatrix w h)

instance (Pos h, Pos w) ⇒ Arbitrary (ObstacleMatrix w h) where
  arbitrary ∷ Gen (ObstacleMatrix w h)
  arbitrary = pure $ ObstacleMatrix $ Vec.fill \_ → Vec.fill \_ → false

instance (Pos x, Pos y) ⇒ Show (ObstacleMatrix x y) where
  show = capLength <<< showFull
    where
    capLength :: String -> String
    capLength s = if S.length s < 21 then s else S.take 18 s <> "..."

    showFull :: ObstacleMatrix x y -> String
    showFull = S.joinWith "\n" <<< map renderRow <<< obstacleMatrixToArray

    renderRow ∷ Array Boolean → String
    renderRow = S.joinWith "" <<< map (if _ then "[#]" else "[ ]")

instance (Pos x, Pos y) ⇒ WriteForeign (ObstacleMatrix x y) where
  writeImpl ∷ ObstacleMatrix x y → Foreign
  writeImpl = writeImpl <<< obstacleMatrixToArray

emptyObstacleMatrix ∷ ∀ h w. Pos h ⇒ Pos w ⇒ ObstacleMatrix w h
emptyObstacleMatrix = ObstacleMatrix $ Vec.fill \_ → Vec.fill \_ → false

obstacleMatrixToArray
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ ObstacleMatrix w h → Array (Array Boolean)
obstacleMatrixToArray (ObstacleMatrix rows) =
  V.toArray <$> V.toArray rows


module Scriptzzz.PathFinding (MapMatrix(..), findPath) where

import Prelude

import Data.String as S
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either.Nested (type (\/))
import Data.Traversable (traverse)
import Data.Typelevel.Num (class Nat)
import Data.Vec (Vec, toArray)
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail)
import Foreign as F
import Scriptzzz.Game.Types (Position)
import Yoga.JSON (class WriteForeign, writeImpl)

newtype MapMatrix :: forall k1 k2. k1 -> k2 -> Type
newtype MapMatrix x y = MapMatrix (Vec x (Vec y Boolean))

derive newtype instance (Nat x, Nat y) => Eq (MapMatrix x y)

instance (Nat x, Nat y) => Show (MapMatrix x y) where
  show = S.joinWith "\n" <<< map renderRow <<< matrixToArray 
    where
    renderRow :: Array Boolean -> String
    renderRow = S.joinWith "" <<< map (if _ then "#" else " ")

instance (Nat x, Nat y) => WriteForeign (MapMatrix x y) where
  writeImpl ∷  MapMatrix x y → Foreign
  writeImpl = writeImpl <<< matrixToArray

foreign import findPathImpl ∷ Array (Array Int) -> Position -> Position -> Foreign

matrixToArray :: forall x y. Nat x => Nat y => MapMatrix x y -> Array (Array Boolean)
matrixToArray (MapMatrix rows) = toArray <$> toArray rows  

findPath :: forall x y. Nat x => Nat y => MapMatrix x y -> Position -> Position -> MultipleErrors \/ Array Position
findPath mapMatrix sourcePosition targetPosition = 
  runExcept $ parseSteps $ findPathImpl (convertMapMatrix mapMatrix) sourcePosition targetPosition 
  where
  convertMapMatrix :: MapMatrix x y -> Array (Array Int)
  convertMapMatrix  = map (map if _ then 1 else 0) <<< matrixToArray

  parseSteps :: Foreign -> F (Array Position)
  parseSteps value = do
    stepTuples <- F.readArray value
    traverse (\tuple -> F.readArray tuple >>= parsePosition) stepTuples
    where
    parsePosition :: Array Foreign -> F Position
    parsePosition = case _ of
      [xValue, yValue] -> do
        x <- F.readInt xValue
        y <- F.readInt yValue
        pure {x, y}
      xs -> 
        fail $ ForeignError $ "Step tuple has a length different from 2: " <> show (Array.length xs)  
      
    
     


module Scriptzzz.Canvas
  ( createCanvas
  , createEntity
  , destroyEntity
  , updateEntityPosition
  ) where

import Scriptzzz.Prelude

import Data.Typelevel.Num (toInt)
import Data.Typelevel.Undefined (undefined)
import Scriptzzz.Core (Id, Position, idToString)
import Scriptzzz.PathFinding as PF
import Web.DOM (Node)

foreign import createCanvasImpl ∷ Int -> Int -> Int -> Int -> Effect Unit → Effect Node

foreign import destroyEntityImpl ∷  String  → Effect Unit

foreign import createEntityImpl
  ∷ forall h w. String → String → Position w h → Effect Unit

foreign import updateEntityPositionImpl
  ∷ forall h w. String → Position w h → Effect Unit

createCanvas ∷ forall w h. Pos w => Pos h => PF.ObstacleMatrix w h -> Effect Unit → Effect Node
createCanvas _ = createCanvasImpl gridWidth gridHeight pixelWidth pixelHeight 
  where
  pixelWidth :: Int
  pixelWidth = gridWidth * cellSize
  
  pixelHeight :: Int
  pixelHeight = gridHeight * cellSize

  gridWidth :: Int
  gridWidth = toInt ( undefined :: w )
  
  gridHeight :: Int
  gridHeight = toInt ( undefined :: h)

  cellSize :: Int
  cellSize = 32

createEntity ∷ forall h w. Pos h => Pos w => Id → String → Position w h → Aff Unit
createEntity id entityType position = liftEffect
  $ createEntityImpl (idToString id) entityType position

destroyEntity ∷ Id → Aff Unit
destroyEntity id = liftEffect $ destroyEntityImpl (idToString id) 

updateEntityPosition ∷ forall h w. Id → Position w h → Aff Unit
updateEntityPosition id position = liftEffect
  $ updateEntityPositionImpl (idToString id) position


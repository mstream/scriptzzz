module Scriptzzz.Canvas
  ( createCanvas
  , createEntity
  , updateEntityPosition
  ) where

import Scriptzzz.Prelude

import Scriptzzz.Core (Id, Position)
import Web.DOM (Node)

foreign import createCanvasImpl ∷ Effect Unit → Effect Node
foreign import createEntityImpl
  ∷ String → String → Position → Effect Unit

foreign import updateEntityPositionImpl
  ∷ String → Position → Effect Unit

createCanvas ∷ Effect Unit → Effect Node
createCanvas = createCanvasImpl

createEntity ∷ Id → String → Position → Aff Unit
createEntity id entityType position = liftEffect
  $ createEntityImpl (show id) entityType position

updateEntityPosition ∷ Id → Position → Aff Unit
updateEntityPosition id position = liftEffect
  $ updateEntityPositionImpl (show id) position


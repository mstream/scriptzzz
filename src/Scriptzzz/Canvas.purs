module Scriptzzz.Canvas (createCanvas, createEntity, updateEntityPosition) where

import Prelude

import Data.String.NonEmpty as NES
import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Scriptzzz.Engine (Id(..))

type Position = {x :: Int, y :: Int}

foreign import createCanvasImpl :: String -> Effect (Promise Unit)
foreign import createEntityImpl :: Fn2 String Position (Effect  Unit)
foreign import updateEntityPositionImpl :: Fn2 String Position (Effect Unit)

createCanvas :: String -> Aff Unit
createCanvas = toAffE <<< createCanvasImpl

createEntity :: Id -> Position -> Aff Unit
createEntity (Id id) position = liftEffect $ runFn2 createEntityImpl (NES.toString id) position 

updateEntityPosition :: Id -> Position -> Aff Unit
updateEntityPosition (Id id) position = liftEffect $ runFn2 updateEntityPositionImpl (NES.toString id) position 

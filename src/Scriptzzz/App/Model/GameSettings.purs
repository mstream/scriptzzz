module Scriptzzz.App.Model.GameSettings
  ( GameSettings
  ) where


import Scriptzzz.Canvas.Animation (FPS)
import Scriptzzz.Game as Game

type GameSettings :: forall k1 k2. k1 -> k2 -> Type
type GameSettings w h =
  { environment ∷ Game.Environment w h
  , initialState :: Game.State w h
  , restartOnScriptChange ∷ Boolean
  , stopOnError ∷ Boolean
  , targetFps :: FPS
  }


module Scriptzzz.App.Model.GameSettings
  ( GameSettings
  ) where


import Scriptzzz.Canvas.Animation (FPS)
import Scriptzzz.Game as Game

type GameSettings =
  { environment ∷ Game.Environment
  , initialState :: Game.State
  , restartOnScriptChange ∷ Boolean
  , stopOnError ∷ Boolean
  , targetFps :: FPS
  }


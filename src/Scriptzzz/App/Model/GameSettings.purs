module Scriptzzz.App.Model.GameSettings
  ( GameSettings
  ) where


import Scriptzzz.Game as Game

type GameSettings =
  { environment ∷ Game.Environment
  , restartOnScriptChange ∷ Boolean
  , stopOnError ∷ Boolean
  }


module Scriptzzz.App.Model
  ( Model
  ) where

import Scriptzzz.App.Model.EditorState (EditorState)
import Scriptzzz.App.Model.GameSettings (GameSettings)
import Scriptzzz.Game as Game

type Model =
  { editorState ∷ EditorState
  , gameLogs :: Game.Logs
  , gameSettings ∷ GameSettings
  , gameState ∷ Game.State
  }


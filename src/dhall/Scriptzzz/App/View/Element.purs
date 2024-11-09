module Scriptzzz.App.View.Element (canvas, editor) where

import Scriptzzz.Prelude

import Data.String.NonEmpty as NES
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Scriptzzz.App.Message as Msg
import Scriptzzz.Canvas (createCanvas)
import Scriptzzz.Core (makeScript)
import Scriptzzz.Editor (createEditor)
import Scriptzzz.Global (sendMessage)
import Scriptzzz.PathFinding as PF

canvas
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ PF.ObstacleMatrix w h
  → Html (Msg.Message w h)
canvas obstacleMatrix = HE.managed_ nodeRenderer unit
  where
  nodeRenderer ∷ HE.NodeRenderer Unit
  nodeRenderer =
    { createNode: const $ createCanvas obstacleMatrix do
        message ← createCanvasInitializedMessage
        sendMessage message
    , updateNode: \n _ _ → pure n
    }

  createCanvasInitializedMessage ∷ Effect (Msg.Message w h)
  createCanvasInitializedMessage =
    Msg.createWithTimestamp $ Msg.CanvasInitialized unit

editor ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Boolean → Html (Msg.Message w h)
editor isVisible = HE.managed nodeRenderer
  [ HA.style { display: if isVisible then "block" else "none" } ]
  unit
  where
  nodeRenderer ∷ HE.NodeRenderer Unit
  nodeRenderer =
    let
      contentsUpdateCallback ∷ String → Effect Unit
      contentsUpdateCallback = \updatedContents → do
        message ← createUpdateMessage updatedContents
        sendMessage message

    in
      { createNode: const $ createEditor
          (NES.nes (Proxy ∷ _ "editor"))
          contentsUpdateCallback
          ""
      , updateNode: \n _ _ → pure n
      }

  createUpdateMessage ∷ String → Effect (Msg.Message w h)
  createUpdateMessage =
    Msg.createWithTimestamp <<< Msg.EditorUpdated <<< makeScript


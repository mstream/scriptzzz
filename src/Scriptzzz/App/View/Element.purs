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

canvas ∷ Html Msg.Message
canvas = HE.managed_ nodeRenderer unit
  where
  nodeRenderer ∷ HE.NodeRenderer Unit
  nodeRenderer =
    { createNode: const $ createCanvas do
        message ← Msg.createWithTimestamp
          $ Msg.CanvasInitialized unit
        sendMessage message
    , updateNode: \n _ _ → pure n
    }

editor ∷ Boolean → Html Msg.Message
editor isVisible = HE.managed nodeRenderer
  [ HA.style { display: if isVisible then "block" else "none" } ]
  unit
  where
  nodeRenderer ∷ HE.NodeRenderer Unit
  nodeRenderer =
    let
      contentsUpdateCallback ∷ String → Effect Unit
      contentsUpdateCallback = \updatedContents → do
        message ← Msg.createWithTimestamp
          $ Msg.EditorUpdated
          $ makeScript updatedContents

        sendMessage message

    in
      { createNode: const $ createEditor
          (NES.nes (Proxy ∷ _ "editor"))
          contentsUpdateCallback
          ""
      , updateNode: \n _ _ → pure n
      }


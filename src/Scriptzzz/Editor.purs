module Scriptzzz.Editor (createEditor) where

import Scriptzzz.Prelude

import Data.String.NonEmpty as NES
import Web.DOM as D
import Web.DOM.Document as DD
import Web.DOM.Element as DE
import Web.HTML as H
import Web.HTML.HTMLDocument as HD
import Web.HTML.Window as HW

foreign import createEditorImpl
  ∷ D.Element → (String → Effect Unit) → String → Effect Unit

createEditor
  ∷ NonEmptyString → (String → Effect Unit) → String → Effect D.Node
createEditor parentElementId contentsChangeCallback initialDocumentText =
  do
    document ← HW.document =<< H.window

    parentElement ← DD.createElement
      "div"
      (HD.toDocument document)

    DE.setId (NES.toString parentElementId) parentElement

    createEditorImpl
      parentElement
      contentsChangeCallback
      initialDocumentText

    pure $ DE.toNode parentElement


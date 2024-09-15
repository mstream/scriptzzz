module Scriptzzz.Editor (createEditor) where

import Prelude

import Effect (Effect)
import Web.DOM (Node)

foreign import createEditorImpl :: (String -> Effect Unit) -> Effect Node

createEditor :: (String -> Effect Unit) -> Effect Node
createEditor = createEditorImpl  

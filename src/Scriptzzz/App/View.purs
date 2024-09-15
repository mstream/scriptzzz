module Scriptzzz.App.View where

import Prelude

import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..), maybe)
import Effect.Now (now)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Scriptzzz.App.Message (Message(..))
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.Editor (createEditor)
import Scriptzzz.Global (sendMessage)

view ∷ Model → Html Message
view = do
  canvasColumn ← canvasColumnView
  codeColumn ← codeColumnView
  debugColumn ← debugColumnView
  pure $ HE.section_
    [ HE.main "main"
        [ HE.div
            [ HA.class' "columns" ]
            [ codeColumn, canvasColumn, debugColumn ]
        ]
    ]

canvasColumnView ∷ Model → Html Message
canvasColumnView = pure $ HE.div
  [ HA.class' "column" ]
  [ HE.text "<CANVAS>" ]

codeColumnView ∷ Model → Html Message
codeColumnView = do
  model <- ask
  pure $ HE.div
    [ HA.class' "column" ]
    [ HE.managed_
        { createNode: const $ createEditor \updatedContents → do
            currentTime ← now
            sendMessage $ EditorUpdated
              { time: currentTime, value: updatedContents }
        , updateNode: \n _ _ → pure n
        }
        unit
    , HE.div_ [
      HE.text case model of
        Evaluating _ →
          "evaluating..."
        Idle payload →
          case payload of
            Nothing →
              ""
            Just { executionResult } →
              show executionResult
        Typing _ →
          "typing..."

      ]
    ]

debugColumnView ∷ Model → Html Message
debugColumnView = do
  model ← ask
  pure $ HE.div
    [ HA.class' "column" ]
    [ HE.text case model of  
        Idle mbState ->
         maybe "" (show <<< _.gameState) mbState
        _ -> 
          ""
    ]


module Scriptzzz.App.View where

import Prelude

import Control.Monad.Reader (ask)
import Effect.Now (now)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Scriptzzz.App.Message (Message(..))
import Scriptzzz.App.Model (Model)
import Scriptzzz.App.Model.EditorState (EditorState(..), Script(..))
import Scriptzzz.Editor (createEditor)
import Scriptzzz.Global (sendMessage)

view ∷ Model → Html Message
view = do
  canvasColumn ← canvasColumnView
  editorColumn ← editorColumnView
  debugColumn ← debugColumnView
  pure $ HE.section_
    [ HE.main "main"
        [ HE.div
            [ HA.class' "columns" ]
            [ editorColumn, canvasColumn, debugColumn ]
        ]
    ]

canvasColumnView ∷ Model → Html Message
canvasColumnView = pure $ HE.div
  [ HA.class' "column" ]
  [ HE.text "<CANVAS>" ]

editorColumnView ∷ Model → Html Message
editorColumnView = do
  model ← ask
  pure $ HE.div
    [ HA.class' "column" ]
    [ HE.managed_
        { createNode: const $ createEditor \updatedContents → do
            currentTime ← now
            sendMessage $ EditorUpdated
              { time: currentTime, value: Script updatedContents }
        , updateNode: \n _ _ → pure n
        }
        unit
    , HE.div_
        [ case model.editorState of
            ExecutingScript _ →
              HE.text "⧖" 
            Idle { script, scriptExecutionOutcome } →
              HE.div_
                [ HE.text $ show scriptExecutionOutcome
                ]
            Typing _ →
              HE.text "⌨" 
        ]
    ]

debugColumnView ∷ Model → Html Message
debugColumnView = do
  model ← ask
  pure $ HE.div
    [ HA.class' "column" ]
    [ HE.text $ show model.gameState, HE.text $ show model.gameLogs ]


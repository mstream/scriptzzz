module Scriptzzz.App.View where

import Prelude

import Control.Monad.Reader (ask)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.App.View.Element (canvas, editor)

view ∷ Model → Html Msg.Message
view = do
  canvasColumnContents ← viewCanvasColumnContents
  editorColumnContents ← viewEditorColumnContents
  debugColumnContents ← viewDebugColumnContents
  pure $ HE.section_
    [ HE.main "main"
        [ HE.div
            [ HA.class' "columns" ]
            ( HE.div [ HA.class' "column" ] <$>
                [ editorColumnContents
                , canvasColumnContents
                , debugColumnContents
                ]
            )
        ]
    ]

viewCanvasColumnContents ∷ Model → Array (Html Msg.Message)
viewCanvasColumnContents = do
  model ← ask
  pure $ [ canvas ] <> case model of
    CanvasInitializing →
      []

    Editing _ →
      [ HE.button
          [ HA.onClick
              $ Msg.createWithoutTimestamp
              $ Msg.SimulationStartRequested unit
          ]
          [ HE.text "Start" ]
      ]

    Simulating _ →
      [ HE.button
          [ HA.onClick
              $ Msg.createWithoutTimestamp
              $ Msg.SimulationStopRequested unit
          ]
          [ HE.text "Stop" ]
      ]

viewEditorColumnContents ∷ Model → Array (Html Msg.Message)
viewEditorColumnContents = do
  model ← ask

  pure case model of
    Editing editingModel →
      [ editor true
      , HE.text $ show editingModel.lastScriptExecution
      ]

    Simulating simulatingModel →
      [ editor false
      , HE.text $ show simulatingModel.editor.lastScriptExecution
      ]
    _ →
      [ editor false, HE.div' [ HA.class' "is-skeleton" ] ]

viewDebugColumnContents ∷ Model → Array (Html Msg.Message)
viewDebugColumnContents = do
  model ← ask
  pure case model of
    Simulating simulatingModel →
      [ HE.text $ show simulatingModel.gameState
      , HE.text $ show simulatingModel.gameLogs
      ]

    _ →
      [ HE.div' [ HA.class' "is-skeleton" ] ]


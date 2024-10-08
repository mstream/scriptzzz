module Scriptzzz.App.View where

import Scriptzzz.Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Scriptzzz.App.Message as Msg
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.App.View.Element (canvas, editor)

view ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Model w h → Html (Msg.Message w h)
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

viewCanvasColumnContents
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Model w h → Array (Html (Msg.Message w h))
viewCanvasColumnContents = do
  model ← ask
  pure case model of
    CanvasInitializing canvasInitializingModel →
      [ canvas canvasInitializingModel.obstacleMatrix ]

    Editing editingModel →
      [ canvas editingModel.gameSettings.environment.obstacleMatrix
      , HE.button
          [ HA.onClick
              $ Msg.createWithoutTimestamp
              $ Msg.SimulationStartRequested unit
          ]
          [ HE.text "Start" ]
      ]

    Simulating simulatingModel →
      [ canvas
          simulatingModel.editor.gameSettings.environment.obstacleMatrix
      , HE.button
          [ HA.onClick
              $ Msg.createWithoutTimestamp
              $ Msg.SimulationStopRequested unit
          ]
          [ HE.text "Stop" ]
      ]

viewEditorColumnContents
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Model w h → Array (Html (Msg.Message w h))
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

viewDebugColumnContents
  ∷ ∀ h w. Pos h ⇒ Pos w ⇒ Model w h → Array (Html (Msg.Message w h))
viewDebugColumnContents = do
  model ← ask
  pure case model of
    Simulating simulatingModel →
      [ HE.text $ show simulatingModel.gameState
      , HE.text $ show simulatingModel.gameLogs
      ]

    _ →
      [ HE.div' [ HA.class' "is-skeleton" ] ]


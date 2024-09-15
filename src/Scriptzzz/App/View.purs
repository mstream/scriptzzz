module Scriptzzz.App.View where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Now (now)
import Flame (Html)
import Flame.Html.Element as HE
import Scriptzzz.App.Message (Message(..))
import Scriptzzz.App.Model (Model(..))
import Scriptzzz.Editor (createEditor)
import Scriptzzz.Global (sendMessage)

view ∷ Model → Html Message
view model = HE.section_
  [ HE.main "main"
      [ HE.managed_
          { createNode: const $ createEditor \updatedContents → do
              currentTime ← now
              sendMessage $ EditorUpdated
                { time: currentTime, value: updatedContents }
          , updateNode: \n _ _ → pure n
          }
          unit
      , HE.text case model of
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


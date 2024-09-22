module Test.Scriptzzz.App.Controller.Handler.ScriptEvaluated (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Vec as Vec
import Scriptzzz.App.Controller.Handler.ScriptEvaluated as ScriptEvaluated
import Scriptzzz.App.Message (ScriptEvaluatedMessage)
import Scriptzzz.App.Model.EditorState (EditorState(..), Script(..))
import Scriptzzz.Game as Game
import Scriptzzz.PathFinding (MapMatrix(..))
import Scriptzzz.Sandbox (ExecutionResult(..))
import Test.HandlerSpec (ScenarioConfiguration, TimeMock, handlerSpec)
import Test.Spec (Spec, describe)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.ScriptEvaluated" do
    scriptEvaluatedSpec "while typing" \{farPast, now, past} ->
     { expectedCommands: { 
            evaluateScript: []
            , logDebug: [{
               effectResult: unit
              , message: Nothing                  
              , parameters: { message: "Ignoring script stale evaluation result" }
            }]
            , logError: []
            }
     , expectedModelResult:  Right { editorState: Typing
              { currentScript: Script "abc"
              , lastUpdateTime: past
              , previousIdleState:
                  { script: Script "", scriptExecutionOutcome: Nothing }
              }
          , gameLogs: []
          , gameSettings:
              { environment:
                  { mapMatrix: MapMatrix $ Vec.fill \_ → Vec.fill \_ →
                      false
                  }
              , restartOnScriptChange: false
              , stopOnError: false
              }
          , gameState: Game.State Map.empty
          }
     , message: {
        executionFinishTime: now
        ,executionResult: Timeout
        ,executionStartTime: past
        , scheduledTime: farPast
     }
     , state: { editorState: Typing
              { currentScript: Script "abc"
              , lastUpdateTime: past
              , previousIdleState:
                  { script: Script "", scriptExecutionOutcome: Nothing }
              }
          , gameLogs: []
          , gameSettings:
              { environment:
                  { mapMatrix: MapMatrix $ Vec.fill \_ → Vec.fill \_ →
                      false
                  }
              , restartOnScriptChange: false
              , stopOnError: false
              }
          , gameState: Game.State Map.empty
          } 
     } 

scriptEvaluatedSpec
  ∷ String
  → (TimeMock → ScenarioConfiguration ScriptEvaluatedMessage)
  → Spec Unit
scriptEvaluatedSpec = handlerSpec "Script Evaluated" ScriptEvaluated.handle


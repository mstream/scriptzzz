module Test.Scriptzzz.App.Controller.Handler.TimeUpdated (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Vec as Vec
import Scriptzzz.App.Controller.Handler.TimeUpdated as TimeUpdated
import Scriptzzz.App.Message (Message(..), TimeUpdatedMessage)
import Scriptzzz.App.Model.EditorState (EditorState(..), Script(..))
import Scriptzzz.Game as Game
import Scriptzzz.PathFinding (MapMatrix(..))
import Scriptzzz.Sandbox (ExecutionResult(..))
import Test.HandlerSpec (ScenarioConfiguration, TimeMock, handlerSpec)
import Test.Spec (Spec, describe)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.TimeUpdated" do
    timeUpdatedSpec "when typing" \{ now, past } →
      { expectedCommands: mempty
      , expectedModelResult:
          Right
            { editorState: Typing
                { currentScript: Script "abc"
                , lastUpdateTime: past
                , previousIdleState:
                    { script: Script ""
                    , scriptExecutionOutcome: Nothing
                    }
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
      , message: now
      , state:
          { editorState: Typing
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

    timeUpdatedSpec "when not typing for a while"
      \{ farFuture, farPast, future, now } →
        { expectedCommands:
            { evaluateScript:
                [ { effectResult:
                      { executionFinishTime: farFuture
                      , executionResult: Timeout
                      , executionStartTime: future
                      }
                  , message: Just $ ScriptEvaluated
                      { executionFinishTime: farFuture
                      , executionResult: Timeout
                      , executionStartTime: future
                      , scheduledTime: now
                      }
                  , parameters: { script: Script "abc" }
                  }
                ]
            , logDebug: []
            , logError: []
            }
        , expectedModelResult:
            Right
              { editorState: ExecutingScript
                  { executionStart: now
                  , previousIdleState:
                      { script: Script ""
                      , scriptExecutionOutcome: Nothing
                      }
                  , script: Script "abc"
                  }
              , gameLogs: []
              , gameSettings:
                  { environment:
                      { mapMatrix: MapMatrix $ Vec.fill \_ → Vec.fill
                          \_ → false
                      }
                  , restartOnScriptChange: false
                  , stopOnError: false
                  }
              , gameState: Game.State Map.empty
              }
        , message: now
        , state:
            { editorState: Typing
                { currentScript: Script "abc"
                , lastUpdateTime: farPast
                , previousIdleState:
                    { script: Script ""
                    , scriptExecutionOutcome: Nothing
                    }
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

timeUpdatedSpec
  ∷ String
  → (TimeMock → ScenarioConfiguration TimeUpdatedMessage)
  → Spec Unit
timeUpdatedSpec = handlerSpec "Time Updated" TimeUpdated.handle


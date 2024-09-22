module Test.Scriptzzz.App.Controller.Handler.EditorUpdated (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Vec as Vec
import Scriptzzz.App.Controller.Handler.EditorUpdated as EditorUpdated
import Scriptzzz.App.Message (EditorUpdatedMessage)
import Scriptzzz.App.Model.EditorState (EditorState(..), Script(..))
import Scriptzzz.Game as Game
import Scriptzzz.PathFinding (MapMatrix(..))
import Test.HandlerSpec (ScenarioConfiguration, TimeMock, handlerSpec)
import Test.Spec (Spec, describe)

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.App.Controller.Handler.EditorUpdated" do
    editorUpdatedSpec "when executing a script" \{ now, past } →
      { expectedCommands: mempty
      , expectedModelResult:
          Right
            { editorState: Typing
                { currentScript: Script "abcdef"
                , lastUpdateTime: now
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
      , message: { time: now, value: Script "abcdef" }
      , state:
          { editorState: ExecutingScript
              { executionStart: past
              , previousIdleState:
                  { script: Script "", scriptExecutionOutcome: Nothing }
              , script: Script "abc"
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

    editorUpdatedSpec "when idling" \{ now } →
      { expectedCommands: mempty
      , expectedModelResult:
          Right
            { editorState: Typing
                { currentScript: Script "abc"
                , lastUpdateTime: now
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
      , message: { time: now, value: Script "abc" }
      , state:
          { editorState: Idle
              { script: Script "", scriptExecutionOutcome: Nothing }
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

    editorUpdatedSpec "when typing" \{ now, past } →
      { expectedCommands: mempty
      , expectedModelResult:
          Right
            { editorState: Typing
                { currentScript: Script "abcdef"
                , lastUpdateTime: now
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
      , message: { time: now, value: Script "abcdef" }
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

editorUpdatedSpec
  ∷ String
  → (TimeMock → ScenarioConfiguration EditorUpdatedMessage)
  → Spec Unit
editorUpdatedSpec = handlerSpec "Editor Updated" EditorUpdated.handle


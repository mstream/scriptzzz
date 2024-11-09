module Test.Scriptzzz.Main where

import Scriptzzz.Prelude

 
import Test.Scriptzzz.Sandbox as Sandbox
import Test.Scriptzzz.Game as Game
import Test.Scriptzzz.App.Controller.Handler.AnimationUpdated as AnimationUpdated
import Test.Scriptzzz.App.Controller.Handler.CanvasInitialized as CanvasInitialized
import Test.Scriptzzz.App.Controller.Handler.EditorUpdated as EditorUpdated
import Test.Scriptzzz.App.Controller.Handler.ScriptExecuted as ScriptExecuted
import Test.Scriptzzz.App.Controller.Handler.SimulationStartRequested as SimulationStartRequested
import Test.Scriptzzz.App.Controller.Handler.SimulationStopRequested as SimulationStopRequested
import Test.Scriptzzz.Game.Command as Command
import Test.Scriptzzz.Canvas.Animation as Animation
import Test.Scriptzzz.App.Controller.Handler.TimeUpdated as TimeUpdated
import Test.Spec (Spec)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Utils (detailedErrorConsoleReporter)
import Test.Scriptzzz.Core as Core

main ∷ Effect Unit
main = runSpecAndExitProcess [ detailedErrorConsoleReporter ] spec

spec ∷ Spec Unit
spec = do
  Animation.spec
  AnimationUpdated.spec
  CanvasInitialized.spec
  Command.spec
  Core.spec
  EditorUpdated.spec
  Game.spec
  TimeUpdated.spec
  Sandbox.spec
  ScriptExecuted.spec
  SimulationStartRequested.spec
  SimulationStopRequested.spec

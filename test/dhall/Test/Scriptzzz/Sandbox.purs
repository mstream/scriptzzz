module Test.Scriptzzz.Sandbox (spec) where

import Scriptzzz.Prelude

import Data.Time.Duration (Milliseconds(..))
import Foreign (MultipleErrors)
import Scriptzzz.Core (makeScript)
import Scriptzzz.Sandbox as Sandbox
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.JSON (class ReadForeign)
import Yoga.JSON as JSON

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.Sandbox" do
    describe "SandboxWorkerMessageData" do
      it "deserializes Exception from JSON properly" do
        let
          json ∷ String
          json = """{"data":"error message","kind":"exception"}"""

          expected ∷ MultipleErrors \/ Sandbox.SandboxWorkerMessageData
          expected = Right $ Sandbox.Exception "error message"

          actual ∷ MultipleErrors \/ Sandbox.SandboxWorkerMessageData
          actual = runExcept $ JSON.readJSON' json

        actual `shouldEqual` expected

      it "deserializes Value from JSON properly" do
        let
          json ∷ String
          json = """{"data":"{\"foo\": 123}","kind":"value"}"""

          expected ∷ MultipleErrors \/ Sandbox.SandboxWorkerMessageData
          expected = Right $ Sandbox.Value """{"foo": 123}"""

          actual ∷ MultipleErrors \/ Sandbox.SandboxWorkerMessageData
          actual = runExcept $ JSON.readJSON' json

        actual `shouldEqual` expected

    describe "runProgram" do
      let
        timelyExecutionDuration ∷ Milliseconds
        timelyExecutionDuration = Milliseconds 10.0

        maximumExecutionDuration ∷ Milliseconds
        maximumExecutionDuration =
          timelyExecutionDuration <> Milliseconds 10.0

        tooLongExecutionDuration ∷ Milliseconds
        tooLongExecutionDuration =
          maximumExecutionDuration <> Milliseconds 10.0

        runFixedSandboxProgram
          ∷ ∀ v
          . ReadForeign v
          ⇒ Sandbox.SandboxTestFixture
          → Aff (Sandbox.ExecutionResult v)
        runFixedSandboxProgram fixture = Sandbox.runProgram
          (Sandbox.runSandboxT fixture)
          maximumExecutionDuration
          (makeScript "FIXED")

      it "returns values of successfully executed programs" do
        let
          fixture ∷ Sandbox.SandboxTestFixture
          fixture = Sandbox.ReturnValue
            { delayDuration: timelyExecutionDuration
            , valueJson: "123"
            }

          expectedExecutionResult ∷ Sandbox.ExecutionResult Int
          expectedExecutionResult = Sandbox.Success 123

        actualExecutionResult ← runFixedSandboxProgram fixture
        actualExecutionResult `shouldEqual` expectedExecutionResult

      it "returns exception of failed programs" do
        let
          fixture ∷ Sandbox.SandboxTestFixture
          fixture = Sandbox.ReturnException
            { delayDuration: timelyExecutionDuration
            , errorMessage: "error message"
            }

          expectedExecutionResult ∷ Sandbox.ExecutionResult Int
          expectedExecutionResult =
            Sandbox.ScriptExecutionError "error message"

        actualExecutionResult ← runFixedSandboxProgram fixture
        actualExecutionResult `shouldEqual` expectedExecutionResult

      it
        "returns timeout and kills a program if it executes for longer than a given limit"
        do
          let
            fixture ∷ Sandbox.SandboxTestFixture
            fixture = Sandbox.ReturnValue
              { delayDuration: tooLongExecutionDuration
              , valueJson: "123"
              }

            expectedExecutionResult ∷ Sandbox.ExecutionResult Int
            expectedExecutionResult =
              Sandbox.ScriptReturnValueNotReceived
                $ Sandbox.ExecutionTimeout maximumExecutionDuration

          actualExecutionResult ← runFixedSandboxProgram fixture
          actualExecutionResult `shouldEqual` expectedExecutionResult

      it "handles web worker initialization properly" do
        let
          fixture ∷ Sandbox.SandboxTestFixture
          fixture = Sandbox.ProduceWebWorkerError

          expectedExecutionResult ∷ Sandbox.ExecutionResult Int
          expectedExecutionResult =
            Sandbox.ScriptReturnValueNotReceived
              $ Sandbox.WebWorkerError "after initialization"

        actualExecutionResult ← runFixedSandboxProgram fixture
        actualExecutionResult `shouldEqual` expectedExecutionResult


module Test.Flame.Update.Handler
  ( class Assert
  , class GetTime
  , ConfigM
  , Expectations
  , HandlerFailureScenarioConfig
  , HandlerSuccessScenarioConfig
  , ModelAssertionConfig
  , ModelAssertionM
  , assertEqual
  , fail
  , nextTimeTickTimestamp
  , runFailureScenario
  , runSuccessScenario
  ) where

import Scriptzzz.Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.List as L
import Data.String as S
import Data.String.NonEmpty as NES
import Effect.Aff (error)
import Effect.Random (randomInt)
import Scriptzzz.App.Controller.Handler (HandleMessage)
import Scriptzzz.Core (Timestamp, unixEpoch)
import Test.QuickCheck (mkSeed)
import Test.QuickCheck.Gen (Gen, evalGen)
import Test.Utils (checkEqual)

class Monad m ⇐ GetTime m where
  nextTimeTickTimestamp ∷ m Timestamp

class MonadTell (List String) m ⇐ Assert m where
  assertEqual
    ∷ ∀ a
    . Eq a
    ⇒ Show a
    ⇒ { actual ∷ a, description ∷ NES.NonEmptyString, expected ∷ a }
    → m Unit

  fail ∷ NES.NonEmptyString → m Unit

newtype ConfigM a = ConfigM (State Timestamp a)

derive newtype instance Applicative ConfigM
derive newtype instance Apply ConfigM
derive newtype instance Bind ConfigM
derive newtype instance Functor ConfigM
derive newtype instance Monad ConfigM
derive newtype instance MonadState Timestamp ConfigM

instance GetTime ConfigM where
  nextTimeTickTimestamp ∷ ConfigM Timestamp
  nextTimeTickTimestamp = modify \currentTimestamp ->
    fromMaybe currentTimestamp (succ currentTimestamp)

runConfigM ∷ ∀ a. ConfigM a → a
runConfigM (ConfigM stateT) = evalState stateT unixEpoch

type ModelAssertionConfig model =
  { nextModel ∷ model, previousModel ∷ model }

newtype ModelAssertionM model a = ModelAssertionM
  (ReaderT (ModelAssertionConfig model) (Writer (List String)) a)

derive newtype instance Applicative (ModelAssertionM model)
derive newtype instance Apply (ModelAssertionM model)
derive newtype instance Bind (ModelAssertionM model)
derive newtype instance Functor (ModelAssertionM model)
derive newtype instance Monad (ModelAssertionM model)

derive newtype instance
  MonadAsk
    (ModelAssertionConfig model)
    (ModelAssertionM model)

derive newtype instance MonadTell (List String) (ModelAssertionM model)

instance Assert (ModelAssertionM model) where
  assertEqual
    ∷ ∀ a
    . Eq a
    ⇒ Show a
    ⇒ { actual ∷ a, description ∷ NES.NonEmptyString, expected ∷ a }
    → (ModelAssertionM model) Unit
  assertEqual { actual, description, expected } =
    when
      (actual /= expected)
      ( tell
          $ L.singleton
          $ NES.toString description
          <> ": "
          <> show actual
          <> " /= "
          <> show expected
      )

  fail ∷ NES.NonEmptyString → (ModelAssertionM model) Unit
  fail = tell <<< L.singleton <<< NES.toString

runModelAssertionM
  ∷ ∀ a model
  . ModelAssertionM model a
  → ModelAssertionConfig model
  → List String
runModelAssertionM (ModelAssertionM readerT) =
  execWriter <<< runReaderT readerT

type Expectations model cmd =
  String \/ ModelAssertionM model Unit /\ cmd

type HandlerScenarioConfig model cmd payload =
  { expectations ∷ Expectations model cmd
  , messagePayload ∷ payload
  , previousModel ∷ model
  }

type HandlerFailureScenarioConfig model payload =
  { expectedErrorMessage ∷ String
  , messagePayload ∷ payload
  , previousModel ∷ model
  }

type HandlerSuccessScenarioConfig model cmd payload =
  { expectedCommands ∷ cmd
  , messagePayload ∷ payload
  , modelExpectations ∷ ModelAssertionM model Unit
  , previousModel ∷ model
  }

runFailureScenario
  ∷ ∀ cmd model payload
  . Show model
  ⇒ HandleMessage model cmd payload
  → Int
  → ConfigM (Gen (HandlerFailureScenarioConfig model payload))
  → Aff Unit
runFailureScenario handleMessage times configM =
  runScenario handleMessage times (map toScenarioConfig <$> configM)
  where
  toScenarioConfig
    ∷ HandlerFailureScenarioConfig model payload
    → HandlerScenarioConfig model cmd payload
  toScenarioConfig
    { expectedErrorMessage, messagePayload, previousModel } =
    { expectations: Left expectedErrorMessage
    , messagePayload
    , previousModel
    }

runSuccessScenario
  ∷ ∀ cmd model payload
  . Show model
  ⇒ HandleMessage model cmd payload
  → Int
  → ConfigM (Gen (HandlerSuccessScenarioConfig model cmd payload))
  → Aff Unit
runSuccessScenario handleMessage times configM =
  runScenario handleMessage times (map toScenarioConfig <$> configM)
  where
  toScenarioConfig
    ∷ HandlerSuccessScenarioConfig model cmd payload
    → HandlerScenarioConfig model cmd payload
  toScenarioConfig
    { expectedCommands
    , messagePayload
    , modelExpectations
    , previousModel
    } =
    { expectations: Right $ modelExpectations /\ expectedCommands
    , messagePayload
    , previousModel
    }

runScenario
  ∷ ∀ cmd model payload
  . Show model
  ⇒ HandleMessage model cmd payload
  → Int
  → ConfigM (Gen (HandlerScenarioConfig model cmd payload))
  → Aff Unit
runScenario handleMessage times configM = do
  when
    (times == 0)
    (throwError $ error $ "Can't run test scenario 0 times")
  tailRecM go times
  where
  go ∷ Int → Aff (Step Int Unit)
  go = case _ of
    0 →
      pure $ Done unit
    n →
      do
        seed ← liftEffect $ mkSeed <$> randomInt bottom top

        let
          genConfig ∷ Gen (HandlerScenarioConfig model cmd payload)
          genConfig = runConfigM configM

          scenarioConfig ∷ HandlerScenarioConfig model cmd payload
          scenarioConfig = evalGen
            genConfig
            { newSeed: seed, size: 10 }

          actualResult ∷ String \/ model /\ cmd
          actualResult = handleMessage
            scenarioConfig.previousModel
            scenarioConfig.messagePayload

        do
          case actualResult, scenarioConfig.expectations of
            Left actualErrorMessage, Left expectedErrorMessage →
              checkEqual
                (NES.nes (Proxy ∷ _ "error message"))
                actualErrorMessage
                expectedErrorMessage
            Left actualErrorMessage, Right _ →
              throwError $ error $ "Unexpected error: " <>
                actualErrorMessage
            Right _, Left expectedErrorMessage →
              throwError $ error $ "Missing expected error: " <>
                expectedErrorMessage
            Right (actualModel /\ actualCommands),
            Right (modelAssertion /\ expectedCommands) → do
              assertModelExpectation
                modelAssertion
                { nextModel: actualModel
                , previousModel: scenarioConfig.previousModel
                }

              checkEqual
                (NES.nes (Proxy ∷ _ "commands"))
                actualCommands
                expectedCommands

          pure $ Loop $ n - 1

assertModelExpectation
  ∷ ∀ model
  . Show model
  ⇒ ModelAssertionM model Unit
  → ModelAssertionConfig model
  → Aff Unit
assertModelExpectation modelAssertionM modelAssertionConfig =
  case runModelAssertionM modelAssertionM modelAssertionConfig of
    Nil →
      pure unit

    assertionErrors →
      throwError $ error $ S.joinWith "\n"
        [ "Model assertion failed: " <> show assertionErrors
        , "Previous model:"
        , show modelAssertionConfig.previousModel
        , "Next model:"
        , show modelAssertionConfig.nextModel
        ]


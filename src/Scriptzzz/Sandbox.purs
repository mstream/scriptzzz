module Scriptzzz.Sandbox
  ( class ManageWebWorker
  , ExecutionResult(..)
  , ParsingErrors
  , SandboxM
  , SandboxT
  , SandboxTestFixture(..)
  , SandboxWorkerMessageData(..)
  , ValueNotReceivedReason(..)
  , ValueParsingFailure
  , onWebWorkerError
  , onWebWorkerMessage
  , postWebWorkerMessage
  , runProgram
  , runSandboxM
  , runSandboxT
  ) where

import Scriptzzz.Prelude

import Data.Array as A
import Data.List.NonEmpty as NEL
import Data.String as S
import Data.Time.Duration
  ( class Duration
  , Milliseconds(..)
  , fromDuration
  )
import Effect.Exception as Exc
import Effect.Ref as Ref
import Foreign (ForeignError(..), MultipleErrors)
import Scriptzzz.Core (Script, scriptToString)
import Scriptzzz.JSON (readForeignTaggedSum, writeForeignTaggedSum)
import Web.Event.Event (Event)
import Web.Worker.MessageEvent (MessageEvent, data_)
import Web.Worker.Worker as Worker
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as JSON
import Yoga.JSON.Error (renderHumanError)

class Monad m ⇐ ManageWebWorker m where
  onWebWorkerError ∷ (Event → Effect Unit) → m Unit
  onWebWorkerMessage ∷ (MessageEvent → Effect Unit) → m Unit
  postWebWorkerMessage ∷ ∀ a. a → m Unit

newtype SandboxM a = SandboxM (ReaderT Worker.Worker Effect a)

derive newtype instance Applicative SandboxM
derive newtype instance Apply SandboxM
derive newtype instance Bind SandboxM
derive newtype instance Functor SandboxM
derive newtype instance Monad SandboxM
derive newtype instance MonadAsk Worker.Worker SandboxM
derive newtype instance MonadEffect SandboxM

instance ManageWebWorker SandboxM where
  onWebWorkerError callback = do
    worker ← ask
    liftEffect $ Worker.onError callback worker
  onWebWorkerMessage callback = do
    worker ← ask
    liftEffect $ Worker.onMessage callback worker
  postWebWorkerMessage message = do
    worker ← ask
    liftEffect $ Worker.postMessage message worker

runSandboxM ∷ ∀ a. String → SandboxM a → Aff a
runSandboxM workerScriptPath (SandboxM readerT) = liftEffect do
  worker ← Worker.new workerScriptPath Worker.defaultWorkerOptions
  runReaderT readerT worker

newtype SandboxT a = SandboxT (ReaderT SandboxTestFixture Aff a)

derive newtype instance Applicative SandboxT
derive newtype instance Apply SandboxT
derive newtype instance Bind SandboxT
derive newtype instance Functor SandboxT
derive newtype instance Monad SandboxT
derive newtype instance MonadAff SandboxT
derive newtype instance MonadAsk SandboxTestFixture SandboxT
derive newtype instance MonadEffect SandboxT

instance ManageWebWorker SandboxT where
  onWebWorkerError ∷ (Event → Effect Unit) → SandboxT Unit
  onWebWorkerError callback = do
    fixture ← ask
    case fixture of
      ProduceWebWorkerError →
        liftEffect $ callback mockErrorEvent

      _ →
        pure unit

  onWebWorkerMessage ∷ (MessageEvent → Effect Unit) → SandboxT Unit
  onWebWorkerMessage callback = do
    fixture ← ask
    case fixture of
      ReturnException { delayDuration, errorMessage } → do
        liftAff $ delay delayDuration
        liftEffect
          $ callback
          $ mockMessageEvent
          $ JSON.write { data: errorMessage, kind: "exception" }

      ReturnValue { delayDuration, valueJson } → do
        liftAff $ delay delayDuration
        liftEffect
          $ callback
          $ mockMessageEvent
          $ JSON.write { data: valueJson, kind: "value" }

      _ →
        pure unit

  postWebWorkerMessage message = do
    pure unit

foreign import mockErrorEventImpl ∷ Event
foreign import mockMessageEventImpl ∷ Foreign → MessageEvent

mockErrorEvent ∷ Event
mockErrorEvent = mockErrorEventImpl

mockMessageEvent ∷ Foreign → MessageEvent
mockMessageEvent = mockMessageEventImpl

data SandboxTestFixture
  = ProduceWebWorkerError
  | ReturnException
      { delayDuration ∷ Milliseconds, errorMessage ∷ String }
  | ReturnValue { delayDuration ∷ Milliseconds, valueJson ∷ String }

runSandboxT ∷ ∀ a. SandboxTestFixture → SandboxT a → Aff a
runSandboxT fixture (SandboxT readerT) = runReaderT readerT fixture

data SandboxWorkerMessageData = Exception String | Value String

derive instance Generic SandboxWorkerMessageData _

instance Eq SandboxWorkerMessageData where
  eq = genericEq

instance ReadForeign SandboxWorkerMessageData where
  readImpl = readForeignTaggedSum

instance Show SandboxWorkerMessageData where
  show = genericShow

readSandboxWorkerMessageData
  ∷ Foreign → MultipleErrors \/ SandboxWorkerMessageData
readSandboxWorkerMessageData = runExcept <<< JSON.read'

data ExecutionResult a
  = InternalParsingError ParsingErrors
  | ScriptExecutionError String
  | ScriptReturnValueNotReceived ValueNotReceivedReason
  | ScriptReturnValueParsingFailure ValueParsingFailure
  | Success a

type ValueParsingFailure =
  { errors ∷ ParsingErrors, valueJson ∷ String }

data ValueNotReceivedReason
  = ExecutionTimeout Milliseconds
  | WebWorkerError String

derive instance Generic ValueNotReceivedReason _

instance Arbitrary ValueNotReceivedReason where
  arbitrary = executionTimeout <|> webWorkerError
    where
    executionTimeout ∷ Gen ValueNotReceivedReason
    executionTimeout = do
      x ← arbitrary
      pure $ ExecutionTimeout $ Milliseconds $ abs x

    webWorkerError ∷ Gen ValueNotReceivedReason
    webWorkerError = WebWorkerError <$> arbitrary

instance Eq ValueNotReceivedReason where
  eq = genericEq

instance Show ValueNotReceivedReason where
  show = genericShow

instance WriteForeign ValueNotReceivedReason where
  writeImpl = writeForeignTaggedSum

derive instance Generic (ExecutionResult a) _

instance (Arbitrary a) ⇒ Arbitrary (ExecutionResult a) where
  arbitrary = genericArbitrary

instance (Eq a) ⇒ Eq (ExecutionResult a)
  where
  eq = genericEq

instance (Show a) ⇒ Show (ExecutionResult a)
  where
  show = genericShow

instance (WriteForeign a) ⇒ WriteForeign (ExecutionResult a) where
  writeImpl = writeForeignTaggedSum

newtype ParsingErrors = ParsingErrors MultipleErrors

derive newtype instance Eq ParsingErrors

instance Arbitrary ParsingErrors where
  arbitrary = (ParsingErrors <<< NEL.singleton) <$> foreignError
    where
    foreignError ∷ Gen ForeignError
    foreignError = ForeignError <$> arbitrary

instance Show ParsingErrors where
  show (ParsingErrors foreignErrors) = S.joinWith
    ", "
    (A.fromFoldable $ renderHumanError <$> foreignErrors)

instance WriteForeign ParsingErrors where
  writeImpl ∷ ParsingErrors → Foreign
  writeImpl (ParsingErrors errors) = JSON.write errorMessages
    where
    errorMessages ∷ Array String
    errorMessages = A.fromFoldable $ show <$> errors

runProgram
  ∷ ∀ d m v
  . Duration d
  ⇒ ManageWebWorker m
  ⇒ MonadEffect m
  ⇒ ReadForeign v
  ⇒ (∀ a. m a → Aff a)
  → d
  → Script
  → Aff (ExecutionResult v)
runProgram executeOutputProgram maxDuration script = do
  foreignValueResultRef ← liftEffect
    $ Ref.new
    $ Left
    $ ExecutionTimeout
    $ fromDuration maxDuration

  outputProgramExecutionFiber ← forkAff
    $ executeOutputProgram
    $ output foreignValueResultRef script

  delay $ fromDuration maxDuration

  killFiber (Exc.error "timed out") outputProgramExecutionFiber

  foreignValueResult ← liftEffect $ Ref.read foreignValueResultRef

  pure case foreignValueResult of
    Left valueNotReceivedReason →
      ScriptReturnValueNotReceived valueNotReceivedReason

    Right foreignValue →
      case readSandboxWorkerMessageData foreignValue of
        Left foreignErrors →
          InternalParsingError $ ParsingErrors foreignErrors

        Right messageData →
          case messageData of
            Exception errorMessage →
              ScriptExecutionError errorMessage

            Value json →
              case runExcept $ JSON.readJSON' json of
                Left foreignErrors →
                  ScriptReturnValueParsingFailure
                    { errors: ParsingErrors foreignErrors
                    , valueJson: json
                    }

                Right value →
                  Success value

output
  ∷ ∀ m
  . MonadEffect m
  ⇒ ManageWebWorker m
  ⇒ Ref (ValueNotReceivedReason \/ Foreign)
  → Script
  → m Unit
output ref script = do
  onWebWorkerError \_ →
    Ref.write (Left $ WebWorkerError "after initialization") ref
  onWebWorkerMessage \messageEvent →
    Ref.write (Right $ data_ messageEvent) ref
  postWebWorkerMessage $ scriptToString script


module Scriptzzz.Sandbox
  ( ExecutionResult(..)
  , ParsingErrors
  , runProgram
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Parallel (parOneOf)
import Control.Promise (Promise, toAffE)
import Data.Array as A
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Extra (snakeCase)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, killFiber)
import Effect.Exception (error, message)
import Foreign (Foreign, MultipleErrors)
import Scriptzzz.JSON (writeForeignTaggedSum)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, writeImpl)
import Yoga.JSON.Generics (genericWriteForeignTaggedSum)

data ExecutionResult a
  = Success a
  | ParsingError ParsingErrors
  | RuntimeError String
  | Timeout

derive instance Generic (ExecutionResult a) _

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
derive newtype instance Show ParsingErrors

instance WriteForeign ParsingErrors where
  writeImpl ∷ ParsingErrors → Foreign
  writeImpl (ParsingErrors errors) = writeImpl errorMessages
    where
    errorMessages ∷ Array String
    errorMessages = A.fromFoldable $ show <$> errors

foreign import execJsImpl ∷ String → Effect (Promise Foreign)

execJs ∷ String → Aff Foreign
execJs = toAffE <<< execJsImpl

runProgram
  ∷ ∀ a. ReadForeign a ⇒ Milliseconds → String → Aff (ExecutionResult a)
runProgram timeout code = do
  timeoutFiber ← forkAff do
    delay timeout
    pure Nothing

  outputFiber ← forkAff do
    result ← try $ execJs code
    pure $ Just result

  mbResult ← parOneOf
    [ joinFiber timeoutFiber, joinFiber outputFiber ]

  killFiber (error "canceling") timeoutFiber
  killFiber (error "canceling") outputFiber

  pure case mbResult of
    Nothing →
      Timeout
    Just result →
      case result of
        Left error →
          RuntimeError $ message error
        Right value →
          case read value of
            Left errors →
              ParsingError (ParsingErrors errors)
            Right json →
              Success json


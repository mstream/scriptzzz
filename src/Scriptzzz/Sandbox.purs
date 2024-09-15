module Scriptzzz.Sandbox (ExecutionResult(..), runProgram) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Parallel (parOneOf)
import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, killFiber)
import Effect.Exception (error, message)
import Foreign (Foreign, MultipleErrors)
import Yoga.JSON (class ReadForeign, read)

data ExecutionResult a
  = Success a
  | ParsingError MultipleErrors
  | RuntimeError String
  | Timeout

derive instance Generic (ExecutionResult a) _

instance (Eq a) => Eq (ExecutionResult a)
  where
  eq = genericEq

instance (Show a) => Show (ExecutionResult a)
  where
  show = genericShow

foreign import execJsImpl ∷ String → Effect (Promise Foreign)

execJs ∷ String → Aff Foreign
execJs = toAffE <<< execJsImpl

runProgram ∷ forall a. ReadForeign a => Milliseconds → String → Aff (ExecutionResult a)
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
    Nothing -> 
      Timeout
    Just result -> 
      case result of
        Left error ->
          RuntimeError $ message error 
        Right value ->
          case read value of
            Left errors ->
              ParsingError errors
            Right json ->
              Success json
           
      

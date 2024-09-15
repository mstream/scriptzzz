module Scriptzzz.Engine (Id(..), Position) where

import Prelude

import Data.Newtype (class Newtype)
import Data.String.NonEmpty (NonEmptyString)
import Yoga.JSON (class ReadForeign, class WriteForeign)

newtype Id = Id NonEmptyString

derive instance Newtype Id _

derive newtype instance Eq Id
derive newtype instance Ord Id
derive newtype instance ReadForeign Id
derive newtype instance Show Id
derive newtype instance WriteForeign Id

type Position = { x :: Int, y :: Int }


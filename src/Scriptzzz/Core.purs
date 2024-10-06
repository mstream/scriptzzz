module Scriptzzz.Core
  ( class MakeId
  , Id
  , Position
  , Script
  , Timestamp
  , idToString
  , makeId
  , makeScript
  , parseId
  , scriptToString
  , timestamp
  , unixEpoch
  ) where

import Scriptzzz.Prelude

import Control.Monad.Gen (chooseInt)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Int as I
import Data.Time.Duration (Milliseconds(..))
import Prim.TypeError as TE
import Test.QuickCheck (class Arbitrary)
import Yoga.JSON (class ReadForeign, class WriteForeign)

newtype Id = Id String

derive newtype instance Arbitrary Id
derive newtype instance Eq Id
derive newtype instance Ord Id
derive newtype instance ReadForeign Id
derive newtype instance Show Id
derive newtype instance WriteForeign Id

class MakeId (s ∷ Symbol) where
  makeId ∷ Proxy s → Id

instance TE.Fail (TE.Text "Cannot create empty ID") ⇒ MakeId "" where
  makeId _ = Id ""
else instance IsSymbol s ⇒ MakeId s where
  makeId p = Id $ reflectSymbol p

idToString ∷ Id → String
idToString (Id s) = s

parseId ∷ String → String \/ Id
parseId = case _ of
  "" →
    Left "Cannot create empty ID"
  s →
    Right $ Id s

type Position = { x ∷ Int, y ∷ Int }

newtype Script = Script String

derive newtype instance Arbitrary Script
derive newtype instance Eq Script
derive newtype instance Monoid Script
derive newtype instance Semigroup Script
derive newtype instance Show Script
derive newtype instance WriteForeign Script

makeScript ∷ String → Script
makeScript = Script

scriptToString ∷ Script → String
scriptToString (Script s) = s

newtype Timestamp = Timestamp Instant

derive newtype instance Eq Timestamp
derive newtype instance Ord Timestamp
derive newtype instance Show Timestamp
derive newtype instance WriteForeign Timestamp

instance Arbitrary Timestamp where
  arbitrary = do
    i ← chooseInt zero top
    pure
      $ Timestamp
      $ fromMaybe bottom (instant $ Milliseconds $ I.toNumber i)

instance Enum Timestamp where
  pred (Timestamp ins) =
    Timestamp <$> (instant $ unInstant ins <> Milliseconds (-1.0))
  succ (Timestamp ins) =
    Timestamp <$> (instant $ unInstant ins <> Milliseconds 1.0)

timestamp ∷ Instant → Timestamp
timestamp = Timestamp

unixEpoch ∷ Timestamp
unixEpoch = Timestamp $ fromMaybe bottom (instant $ Milliseconds zero)


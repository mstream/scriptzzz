module Scriptzzz.Core
  ( class MakeId
  , Direction(..)
  , Id
  , Position
  , Script
  , Timestamp
  , idToString
  , makeId
  , makePosition
  , makeScript
  , move
  , parseId
  , scriptToString
  , timestamp
  , unixEpoch
  ) where

import Scriptzzz.Prelude

import Control.Monad.Gen (chooseInt)
import Data.Array as A
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Int as I
import Data.Time.Duration (Milliseconds(..))
import Data.Typelevel.Num (class Lt, toInt)
import Data.Typelevel.Undefined (undefined)
import Foreign (ForeignError(..))
import Foreign as F
import Prim.TypeError as TE
import Test.QuickCheck (class Arbitrary)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as JSON

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

data Direction
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW

derive instance Generic Direction _

instance Arbitrary Direction where
  arbitrary = genericArbitrary

newtype Position :: forall k1 k2. k1 -> k2 -> Type
newtype Position w h = Position { x ∷ Int, y ∷ Int }

derive newtype instance Eq (Position w h)
derive newtype instance Show (Position w h)
derive newtype instance WriteForeign (Position w h)

instance (Pos h, Pos w) => Arbitrary (Position w h) where
  arbitrary = do
    x <- chooseInt 0 (toInt (undefined :: w) - 1)
    y <- chooseInt 0 (toInt (undefined :: h) - 1)
    pure $ Position {x, y}

instance (Pos h, Pos w) => ReadForeign (Position w h) where
  readImpl f = Position <$> 
    if F.isArray f then readFromArray f else JSON.read' f
    where
    readFromArray :: Foreign -> F {x :: Int, y :: Int}
    readFromArray fs = do 
      values <- F.readArray fs
      case values of
        [ xValue, yValue ] → do
          x ← F.readInt xValue
          y ← F.readInt yValue
          when 
            (x >= toInt (undefined :: w))
            (F.fail $ ForeignError $ "X coordinate too large: " <> show x)
          when 
            (y >= toInt (undefined :: h))
            (F.fail $ ForeignError $ "Y coordinate too large: " <> show y)
          pure { x, y }
        xs →
          F.fail $ ForeignError
            $ "Step tuple has a length different from 2: "
            <> show (A.length xs)

makePosition :: forall h w x y. Pos h => Pos w => Nat x => Nat y => Lt x w => Lt y h => x -> y -> Position w h
makePosition x y = Position {x: toInt x, y: toInt y} 

{- TODO: make type safe boundaries -}
move :: forall w h. Pos h => Pos w => Position w h -> Direction -> Position w h
move (Position p) = Position <<< case _ of
        N →
           p { y = p.y - 1 } 

        NE →
          { x: p.x + 1, y: p.y - 1 } 

        E →
         p { x = p.x + 1 } 

        SE →
           { x: p.x + 1, y: p.y + 1 } 

        S →
          p { y = p.y + 1 } 

        SW →
          { x: p.x - 1, y: p.y + 1 } 

        W →
          p  { x = p.x - 1 } 

        NW →
          { x: p.x - 1, y: p.y - 1 } 



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


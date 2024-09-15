module Scriptzzz.Game.Command
  ( Commands
  , MoveToCommand
  , UnitCommands(..)
  ) where

import Prelude

import Data.Foldable (class Foldable, foldM)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NES
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Index ((!))
import Foreign.Keys (keys)
import Scriptzzz.Game.Types (Id(..), Position)
import Yoga.JSON
  ( class ReadForeign
  , class WriteForeign
  , readImpl
  , writeImpl
  )

type Commands =
  { workers ∷
      { moveTo ∷ UnitCommands MoveToCommand
      }
  }

newtype UnitCommands a = UnitCommands (Map Id a)

derive newtype instance Foldable UnitCommands
derive newtype instance FoldableWithIndex Id UnitCommands
derive newtype instance Show a ⇒ Show (UnitCommands a)

instance ReadForeign a ⇒ ReadForeign (UnitCommands a) where
  readImpl ∷ Foreign → F (UnitCommands a)
  readImpl json = do
    ks ← keys json
    foldM accummulate (UnitCommands Map.empty) ks
    where
    accummulate ∷ UnitCommands a → String → F (UnitCommands a)
    accummulate (UnitCommands acc) key = case NES.fromString key of
      Just nes → do
        commandValue ← json ! key
        command ← readImpl commandValue
        pure $ UnitCommands $ Map.insert (Id nes) command acc
      Nothing →
        fail $ ForeignError "blank unit command ID"

instance WriteForeign a ⇒ WriteForeign (UnitCommands a) where
  writeImpl ∷ UnitCommands a → Foreign
  writeImpl unitCommands =
    writeImpl $ foldlWithIndex accummulate Map.empty unitCommands
    where
    accummulate ∷ Id → Map String a → a → Map String a
    accummulate (Id nes) acc command = Map.insert (NES.toString nes)
      command
      acc

type MoveToCommand = { position ∷ Position }


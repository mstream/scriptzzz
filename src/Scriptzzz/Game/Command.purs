module Scriptzzz.Game.Command
  ( Commands
  , MoveToCommand
  , UnitCommands(..)
  ) where

import Scriptzzz.Prelude

import Data.Map as M
import Foreign (F, ForeignError(..), fail)
import Foreign.Index ((!))
import Foreign.Keys (keys)
import Scriptzzz.Core (Id, Position, idToString, parseId)
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

derive newtype instance Eq a ⇒ Eq (UnitCommands a)
derive newtype instance Foldable UnitCommands
derive newtype instance FoldableWithIndex Id UnitCommands
derive newtype instance Show a ⇒ Show (UnitCommands a)

instance Arbitrary a => Arbitrary (UnitCommands a) where
  arbitrary = do
    id <- arbitrary
    command <- arbitrary
    pure $ UnitCommands $ M.singleton id command

instance ReadForeign a ⇒ ReadForeign (UnitCommands a) where
  readImpl ∷ Foreign → F (UnitCommands a)
  readImpl json = do
    ks ← keys json
    foldM accummulate (UnitCommands M.empty) ks
    where
    accummulate ∷ UnitCommands a → String → F (UnitCommands a)
    accummulate (UnitCommands acc) key = case parseId key of
      Left errorMessage →
        fail $ ForeignError $ "invalid unit command ID: " <>
          errorMessage
      Right id → do
        commandValue ← json ! key
        command ← readImpl commandValue
        pure $ UnitCommands $ M.insert id command acc

instance WriteForeign a ⇒ WriteForeign (UnitCommands a) where
  writeImpl ∷ UnitCommands a → Foreign
  writeImpl unitCommands =
    writeImpl $ foldlWithIndex accummulate M.empty unitCommands
    where
    accummulate ∷ Id → Map String a → a → Map String a
    accummulate id acc command = M.insert
      (idToString id)
      command
      acc

type MoveToCommand = { position ∷ Position }


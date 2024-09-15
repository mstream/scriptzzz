module Scriptzzz.Game (Entity(..), State, UpdateError, update) where

import Prelude

import Control.Monad.RWS (RWS)
import Control.Monad.State (get)
import Control.Monad.Writer (Writer)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Scriptzzz.Game.Command (Commands, MoveToCommand, UnitCommands)
import Scriptzzz.Game.Types (Id, Position)

data Entity
  = EnergySource { position ∷ Position, quantity ∷ Int }
  | Worker { task ∷ Maybe String, position ∷ Position }

derive instance Generic Entity _

instance Show Entity where
  show = genericShow

type State = Map Id Entity

newtype UpdateError = UpdateError String

type UpdateMonad = Writer (List UpdateError)

type Environment = { height ∷ Int, width ∷ Int }

type Logs = List UpdateError

type Game = RWS Environment Logs State

update ∷ Commands → Environment → State → State /\ Logs
update commands environment state = state /\ mempty

{-
  execRWS (moveTo commands.moveTo)
  environment
  state
-}

moveTo ∷ UnitCommands MoveToCommand → Game Unit
moveTo commands = do
  state ← get

  let
    f ∷ Id → Unit → MoveToCommand → Game Unit
    f id _ { position } = do
      pure unit

  foldWithIndexM f unit commands


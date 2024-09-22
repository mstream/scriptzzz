module Scriptzzz.Game
  ( module Command 
  , Entity(..)
  , Environment
  , Logs
  , MovingTask
  , State(..)
  , UpdateError
  , update
  ) where

import Prelude

import Control.Monad.RWS (RWS, execRWS)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Writer (Writer, tell)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D32)
import Foreign.Object (fromFoldable)
import Scriptzzz.Game.Command (Commands, MoveToCommand, UnitCommands(..)) as Command
import Scriptzzz.Game.Types (Id(..), Position)
import Scriptzzz.JSON (writeForeignTaggedSum)
import Scriptzzz.PathFinding (MapMatrix, findPath)
import Yoga.JSON (class WriteForeign, writeImpl)

type MovingTask = { steps ∷ Array Position, targetPosition ∷ Position }

data Entity
  = EnergySource { position ∷ Position, quantity ∷ Int }
  | Worker { task ∷ Maybe MovingTask, position ∷ Position }

derive instance Generic Entity _

instance Eq Entity where
  eq = genericEq

instance Show Entity where
  show = genericShow

instance WriteForeign Entity where
  writeImpl = writeForeignTaggedSum

newtype State = State (Map Id Entity)

derive newtype instance Eq State
derive newtype instance Show State

instance WriteForeign State where
  writeImpl (State state) = writeImpl $ fromFoldable $ objectEntries
    where
    objectEntries ∷ List (String /\ Entity)
    objectEntries = Map.toUnfoldable state <#> \((Id id) /\ entity) →
      NES.toString id /\ entity

newtype UpdateError = UpdateError String

derive newtype instance Eq UpdateError

derive newtype instance Show UpdateError

derive newtype instance WriteForeign UpdateError

type UpdateMonad = Writer (List UpdateError)

type Environment = { mapMatrix ∷ MapMatrix D32 D32 }

type Logs = Array UpdateError

type Game = RWS Environment Logs State

update ∷ Command.Commands → Environment → State → State /\ Logs
update commands environment state = execRWS
  (moveTo commands.workers.moveTo)
  environment
  state

moveTo ∷ Command.UnitCommands Command.MoveToCommand → Game Unit
moveTo commands = do
  (State state) ← get

  let
    f ∷ Id → Unit → Command.MoveToCommand → Game Unit
    f id _ { position } = case Map.lookup id state of
      Just entity →
        case entity of
          EnergySource _ →
            tell
              [ UpdateError $
                  "cannot move a structure entity: " <> show id
              ]

          Worker workerProps → do
            { mapMatrix } ← ask
            case findPath mapMatrix workerProps.position position of
              Left errors →
                tell
                  [ UpdateError $
                      "cannot calculate path: " <> show
                        errors
                  ]
              Right steps →
                put $ State $ Map.insert
                  id
                  ( Worker workerProps
                      { task = Just { steps, targetPosition: position }
                      }
                  )
                  state

      Nothing →
        tell
          [ UpdateError $
              "cannot move non-existion entity: " <> show
                id
          ]

  foldWithIndexM f unit commands


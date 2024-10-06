module Scriptzzz.Game
  ( module Command
  , Entity(..)
  , Environment
  , Logs
  , MovingTask
  , State(..)
  , UpdateError
  , blankState
  , update
  ) where

import Scriptzzz.Prelude

import Control.Monad.RWS (RWS, execRWS)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Map as M
import Data.Typelevel.Num (D32)
import Foreign.Object (fromFoldable)
import Scriptzzz.Core (Id, Position)
import Scriptzzz.Game.Command
  ( Commands
  , MoveToCommand
  , UnitCommands(..)
  ) as Command
import Scriptzzz.JSON (writeForeignTaggedSum)
import Scriptzzz.PathFinding as PF
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Yoga.JSON (class WriteForeign, writeImpl)

type MovingTask =
  { path ∷ PF.Path, targetPosition ∷ Position }

data Entity
  = EnergySource { position ∷ Position, quantity ∷ Int }
  | Trail { position ∷ Position }
  | Worker { task ∷ Maybe MovingTask, position ∷ Position }

derive instance Generic Entity _

instance Arbitrary Entity where
  arbitrary = genericArbitrary

instance Eq Entity where
  eq = genericEq

instance Show Entity where
  show = genericShow

instance WriteForeign Entity where
  writeImpl = writeForeignTaggedSum

newtype State = State (Map Id Entity)

derive newtype instance Eq State
derive newtype instance Show State

instance Arbitrary State where
  arbitrary = do
    id ← arbitrary
    entity ← arbitrary
    pure $ State $ M.singleton id entity

instance WriteForeign State where
  writeImpl (State state) = writeImpl $ fromFoldable $ objectEntries
    where
    objectEntries ∷ List (String /\ Entity)
    objectEntries = M.toUnfoldable state <#>
      \(id /\ entity) →
        show id /\ entity

newtype UpdateError = UpdateError String

derive newtype instance Arbitrary UpdateError
derive newtype instance Eq UpdateError
derive newtype instance Show UpdateError
derive newtype instance WriteForeign UpdateError

type UpdateMonad = Writer (List UpdateError)

type Environment =
  { obstacleMatrix ∷ PF.ObstacleMatrix D32 D32 }

type Logs = Array UpdateError

type Game = RWS Environment Logs State

update ∷ Environment -> Command.Commands → State → State /\ Logs
update environment commands state = execRWS
  (moveTo commands.workers.moveTo)
  environment
  state

moveTo ∷ Command.UnitCommands Command.MoveToCommand → Game Unit
moveTo commands = do
  State state ← get

  let
    f ∷ Id → Unit → Command.MoveToCommand → Game Unit
    f id _ { position } = case M.lookup id state of
      Just entity →
        case entity of
          EnergySource _ →
            tell
              [ UpdateError
                  $ "cannot move a structure entity: "
                  <> show id
              ]

          Trail _ →
            tell
              [ UpdateError
                  $ "cannot move a hint entity: "
                  <> show id
              ]

          Worker workerProps → do
            { obstacleMatrix } ← ask
            case
              PF.findPath
                obstacleMatrix
                workerProps.position
                position
              of
              Left errors →
                tell
                  [ UpdateError
                      $ "cannot calculate path: "
                      <> show
                        errors
                  ]
              Right path →
                case PF.followPath path >>= (PF.followPath <<< snd) of
                  Just (nextPosition /\ nextPath) →
                    put $ State $ M.insert
                      id
                      ( Worker workerProps
                          { position = nextPosition 
                          , task = Just
                              { path: nextPath
                              , targetPosition: position
                              }
                          }
                      )
                      state
                  Nothing →
                    pure unit

      Nothing →
        tell
          [ UpdateError
              $ "cannot move non-existion entity: "
              <> show
                id
          ]

  foldWithIndexM f unit commands

blankState ∷ State
blankState = State M.empty

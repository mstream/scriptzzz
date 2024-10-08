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
import Foreign.Object (fromFoldable)
import Scriptzzz.Core (Id, Position)
import Scriptzzz.Game.Command
  ( Commands
  , MoveToCommand
  , UnitCommands(..)
  ) as Command
import Scriptzzz.JSON (writeForeignTaggedSum)
import Scriptzzz.PathFinding (Path)
import Scriptzzz.PathFinding as PF
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Yoga.JSON (class WriteForeign, writeImpl)

type MovingTask ∷ ∀ k1 k2. k1 → k2 → Type
type MovingTask w h =
  { path ∷ Path w h, targetPosition ∷ Position w h }

data Entity ∷ ∀ k1 k2. k1 → k2 → Type
data Entity w h
  = EnergySource { position ∷ Position w h, quantity ∷ Int }
  | Trail { position ∷ Position w h }
  | Worker { task ∷ Maybe (MovingTask w h), position ∷ Position w h }

derive instance Generic (Entity w h) _

instance (Pos h, Pos w) ⇒ Arbitrary (Entity w h) where
  arbitrary = genericArbitrary

instance Eq (Entity w h) where
  eq = genericEq

instance Show (Entity w h) where
  show = genericShow

instance WriteForeign (Entity w h) where
  writeImpl = writeForeignTaggedSum

newtype State ∷ ∀ k1 k2. k1 → k2 → Type
newtype State w h = State (Map Id (Entity w h))

derive newtype instance Eq (State w h)
derive newtype instance Show (State w h)

instance (Pos h, Pos w) ⇒ Arbitrary (State w h) where
  arbitrary = do
    id ← arbitrary
    entity ← arbitrary
    pure $ State $ M.singleton id entity

instance WriteForeign (State w h) where
  writeImpl (State state) = writeImpl $ fromFoldable $ objectEntries
    where
    objectEntries ∷ List (String /\ (Entity w h))
    objectEntries = M.toUnfoldable state <#>
      \(id /\ entity) →
        show id /\ entity

newtype UpdateError = UpdateError String

derive newtype instance Arbitrary UpdateError
derive newtype instance Eq UpdateError
derive newtype instance Show UpdateError
derive newtype instance WriteForeign UpdateError

type UpdateMonad = Writer (List UpdateError)

type Environment ∷ ∀ k1 k2. k1 → k2 → Type
type Environment w h =
  { obstacleMatrix ∷ PF.ObstacleMatrix w h }

type Logs = Array UpdateError

type Game ∷ ∀ k1 k2. k1 → k2 → Type → Type
type Game w h = RWS (Environment w h) Logs (State w h)

update
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Environment w h
  → Command.Commands w h
  → State w h
  → State w h /\ Logs
update environment commands state = execRWS
  (moveTo commands.workers.moveTo)
  environment
  state

moveTo
  ∷ ∀ h w
  . Pos h
  ⇒ Pos w
  ⇒ Command.UnitCommands (Command.MoveToCommand w h)
  → Game w h Unit
moveTo commands = do
  State state ← get

  let
    f ∷ Id → Unit → Command.MoveToCommand w h → Game w h Unit
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

blankState ∷ ∀ h w. Pos h ⇒ Pos w ⇒ State w h
blankState = State M.empty

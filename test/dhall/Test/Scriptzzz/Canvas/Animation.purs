module Test.Scriptzzz.Canvas.Animation (spec) where

import Scriptzzz.Prelude

import Data.Map as M
import Data.Typelevel.Num (D4, d0, d1, d2, d3)
import Scriptzzz.Canvas.Animation (Animation, animate)
import Scriptzzz.Core (Id, makeId, makePosition)
import Scriptzzz.Game as Game
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

e1 ∷ Id
e1 = makeId (Proxy ∷ _ "e1")

e2 ∷ Id
e2 = makeId (Proxy ∷ _ "e2")

spec ∷ Spec Unit
spec = do
  describe "Scriptzzz.Canvas.Animation" do
    describe "animate" do
      it "creates entities" do
        let
          previousGameState ∷ Game.State D4 D4
          previousGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: makePosition d0 d1 }
            ]

          nextGameState ∷ Game.State D4 D4
          nextGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: makePosition d0 d1 }
            , e2 /\ Game.Worker
                { task: Nothing, position: makePosition d2 d3 }
            ]

          actual ∷ Animation D4 D4
          actual = animate previousGameState nextGameState

          expected ∷ Animation D4 D4
          expected =
            { createEntity:
                [ { entityType: "worker"
                  , id: e2
                  , position: makePosition d2 d3
                  }
                ]
            , destroyEntity: []
            , updateEntity: []
            }

        actual `shouldEqual` expected

      it "destroys entities" do
        let
          previousGameState ∷ Game.State D4 D4
          previousGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: makePosition d0 d1 }
            , e2 /\ Game.Worker
                { task: Nothing, position: makePosition d1 d2 }
            ]

          nextGameState ∷ Game.State D4 D4
          nextGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: makePosition d0 d1 }
            ]

          actual ∷ Animation D4 D4
          actual = animate previousGameState nextGameState

          expected ∷ Animation D4 D4
          expected =
            { createEntity: []
            , destroyEntity: [ { id: e2 } ]
            , updateEntity: []
            }

        actual `shouldEqual` expected

      it "updates entities" do
        let
          previousGameState ∷ Game.State D4 D4
          previousGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: makePosition d0 d1 }
            , e2 /\ Game.Worker
                { task: Nothing, position: makePosition d1 d2 }
            ]

          nextGameState ∷ Game.State D4 D4
          nextGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: makePosition d0 d1 }
            , e2 /\ Game.Worker
                { task: Nothing, position: makePosition d2 d3 }
            ]

          actual ∷ Animation D4 D4
          actual = animate previousGameState nextGameState

          expected ∷ Animation D4 D4
          expected =
            { createEntity: []
            , destroyEntity: []
            , updateEntity:
                [ { id: e2
                  , sourcePosition: makePosition d1 d2
                  , targetPosition: makePosition d2 d3
                  }
                ]
            }

        actual `shouldEqual` expected


module Test.Scriptzzz.Canvas.Animation (spec) where

import Scriptzzz.Prelude

import Data.Map as M
import Scriptzzz.Canvas.Animation (Animation, animate)
import Scriptzzz.Core (Id, makeId)
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
          previousGameState ∷ Game.State
          previousGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: { x: 1, y: 2 } }
            ]

          nextGameState ∷ Game.State
          nextGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: { x: 1, y: 2 } }
            , e2 /\ Game.Worker
                { task: Nothing, position: { x: 3, y: 4 } }
            ]

          actual ∷ Animation
          actual = animate previousGameState nextGameState

          expected ∷ Animation
          expected =
            { createEntity:
                [ { entityType: "worker"
                  , id: e2
                  , position: { x: 3, y: 4 }
                  }
                ]
            , destroyEntity: []
            , updateEntity: []
            }

        actual `shouldEqual` expected

      it "destroys entities" do
        let
          previousGameState ∷ Game.State
          previousGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: { x: 1, y: 2 } }
            , e2 /\ Game.Worker
                { task: Nothing, position: { x: 3, y: 4 } }
            ]

          nextGameState ∷ Game.State
          nextGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: { x: 1, y: 2 } }
            ]

          actual ∷ Animation
          actual = animate previousGameState nextGameState

          expected ∷ Animation
          expected =
            { createEntity: []
            , destroyEntity: [ { id: e2 } ]
            , updateEntity: []
            }

        actual `shouldEqual` expected

      it "updates entities" do
        let
          previousGameState ∷ Game.State
          previousGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: { x: 1, y: 2 } }
            , e2 /\ Game.Worker
                { task: Nothing, position: { x: 3, y: 4 } }
            ]

          nextGameState ∷ Game.State
          nextGameState = Game.State $ M.fromFoldable
            [ e1 /\ Game.Worker
                { task: Nothing, position: { x: 1, y: 2 } }
            , e2 /\ Game.Worker
                { task: Nothing, position: { x: 5, y: 6 } }
            ]

          actual ∷ Animation
          actual = animate previousGameState nextGameState

          expected ∷ Animation
          expected =
            { createEntity: []
            , destroyEntity: []
            , updateEntity:
                [ { id: e2
                  , sourcePosition: { x: 3, y: 4 }
                  , targetPosition: { x: 5, y: 6 }
                  }
                ]
            }

        actual `shouldEqual` expected


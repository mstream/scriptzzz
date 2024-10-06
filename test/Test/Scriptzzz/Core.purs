module Test.Scriptzzz.Core (spec) where

import Scriptzzz.Prelude

import Scriptzzz.Core (Id, makeId)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.JSON as JSON

spec ∷ Spec Unit
spec = do
  describe "Scriptzz.Core" do
    it "serialized ID to JSON properly" do
      let
        id :: Id
        id = makeId (Proxy :: _ "foo")

        actual :: String 
        actual = JSON.writeJSON id

        expected :: String
        expected = """"foo""""

      actual `shouldEqual` expected 

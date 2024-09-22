module Scriptzzz.JSON (writeForeignTaggedSum) where

import Data.Generic.Rep (class Generic)
import Data.String.Extra (snakeCase)
import Foreign (Foreign)
import Yoga.JSON.Generics
  ( class WriteGenericTaggedSumRep
  , genericWriteForeignTaggedSum
  )

writeForeignTaggedSum
  ∷ ∀ a rep
  . Generic a rep
  ⇒ WriteGenericTaggedSumRep rep
  ⇒ a
  → Foreign
writeForeignTaggedSum = genericWriteForeignTaggedSum
  { typeTag: "kind", valueTag: "data", toConstructorName: snakeCase }


module Scriptzzz.JSON (readForeignTaggedSum,writeForeignTaggedSum) where

import Scriptzzz.Prelude

import Data.String.Extra (snakeCase)
import Yoga.JSON.Generics (class ReadGenericTaggedSumRep, class WriteGenericTaggedSumRep, genericReadForeignTaggedSum, genericWriteForeignTaggedSum)

readForeignTaggedSum
  ∷ ∀ a rep
  . Generic a rep
  ⇒ ReadGenericTaggedSumRep rep
  ⇒ Foreign
  → F a
readForeignTaggedSum = genericReadForeignTaggedSum
  { typeTag: "kind", valueTag: "data", toConstructorName: snakeCase }

writeForeignTaggedSum
  ∷ ∀ a rep
  . Generic a rep
  ⇒ WriteGenericTaggedSumRep rep
  ⇒ a
  → Foreign
writeForeignTaggedSum = genericWriteForeignTaggedSum
  { typeTag: "kind", valueTag: "data", toConstructorName: snakeCase }



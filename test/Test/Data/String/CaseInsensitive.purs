module Test.Data.String.CaseInsensitive (testCaseInsensitiveString) where

import Prelude (Unit, (==), ($), discard, compare, Ordering(..))

import Effect (Effect)
import Effect.Console (log)

import Data.String.CaseInsensitive

import Test.Assert (assert)

testCaseInsensitiveString :: Effect Unit
testCaseInsensitiveString = do
  log "equality"
  assert $ CaseInsensitiveString "aB" == CaseInsensitiveString "AB"

  log "comparison"
  assert $ compare (CaseInsensitiveString "qwerty") (CaseInsensitiveString "QWERTY") == EQ

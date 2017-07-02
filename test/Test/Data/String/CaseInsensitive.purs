module Test.Data.String.CaseInsensitive (testCaseInsensitiveString) where

import Prelude (Unit, (==), ($), discard, compare, Ordering(..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.String.CaseInsensitive

import Test.Assert (ASSERT, assert)

testCaseInsensitiveString :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testCaseInsensitiveString = do
  log "equality"
  assert $ CaseInsensitiveString "aB" == CaseInsensitiveString "AB"

  log "comparison"
  assert $ compare (CaseInsensitiveString "qwerty") (CaseInsensitiveString "QWERTY") == EQ

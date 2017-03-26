module Test.Data.String.Unsafe (testStringUnsafe) where

import Prelude (Unit, (==), ($), discard)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.String.Unsafe

import Test.Assert (ASSERT, assert)

testStringUnsafe :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testStringUnsafe = do
  log "charCodeAt"
  assert $ charCodeAt 0 "ab" == 97
  assert $ charCodeAt 1 "ab" == 98

  log "charAt"
  assert $ charAt 0 "ab" == 'a'
  assert $ charAt 1 "ab" == 'b'

  log "char"
  assert $ char "a" == 'a'

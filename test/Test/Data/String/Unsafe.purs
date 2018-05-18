module Test.Data.String.Unsafe (testStringUnsafe) where

import Prelude (Unit, (==), ($), discard)

import Effect (Effect)
import Effect.Console (log)

import Data.String.Unsafe

import Test.Assert (assert)

testStringUnsafe :: Effect Unit
testStringUnsafe = do
  log "charCodeAt"
  assert $ charCodeAt 0 "ab" == 97
  assert $ charCodeAt 1 "ab" == 98

  log "charAt"
  assert $ charAt 0 "ab" == 'a'
  assert $ charAt 1 "ab" == 'b'

  log "char"
  assert $ char "a" == 'a'

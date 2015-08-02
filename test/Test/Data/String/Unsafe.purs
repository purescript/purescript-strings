module Test.Data.String.Unsafe (testStringUnsafe) where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.String.Unsafe
import Test.Assert (assert)

testStringUnsafe = do
  log "charCodeAt"
  assert $ charCodeAt 0 "ab" == 97
  assert $ charCodeAt 1 "ab" == 98

  log "charAt"
  assert $ charAt 0 "ab" == 'a'
  assert $ charAt 1 "ab" == 'b'

  log "char"
  assert $ char "a" == 'a'

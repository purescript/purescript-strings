module Test.Data.Char (testChar) where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Char
import Test.Assert (assert)

testChar = do
  log "toString"
  assert $ toString 'a' == "a"

  log "toCharCode"
  assert $ toCharCode 'a' == 97
  assert $ toCharCode '\n' == 10

  log "fromCharCode"
  assert $ fromCharCode 97 == 'a'
  assert $ fromCharCode 10 == '\n'

  log "toLower"
  assert $ toLower 'A' == 'a'
  assert $ toLower 'a' == 'a'

  log "toUpper"
  assert $ toUpper 'a' == 'A'
  assert $ toUpper 'A' == 'A'


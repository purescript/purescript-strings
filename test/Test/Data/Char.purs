module Test.Data.Char (testChar) where

import Prelude (Unit, (==), ($), discard)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Char

import Test.Assert (ASSERT, assert)

testChar :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testChar = do
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

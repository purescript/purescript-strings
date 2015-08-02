module Test.Data.String.Regex (testStringRegex) where

import Prelude
import Data.Maybe
import Control.Monad.Eff.Console (log)
import Data.String.Regex
import Test.Assert (assert)

testStringRegex = do
  log "regex"
  assert $ test (regex "^a" noFlags) "abc"
  assert $ not (test (regex "^b" noFlags) "abc")

  log "match"
  assert $ match (regex "^abc$" noFlags) "abc" == Just [Just "abc"]

  log "replace"
  assert $ replace (regex "-" noFlags) "!" "a-b-c" == "a!b-c"

  log "replace'"
  assert $ replace' (regex "-" noFlags) (\s xs -> "!") "a-b-c" == "a!b-c"

  log "search"
  assert $ search (regex "b" noFlags) "abc" == Just 1
  assert $ search (regex "d" noFlags) "abc" == Nothing

  log "split"
  assert $ split (regex "" noFlags) "" == []
  assert $ split (regex "" noFlags) "abc" == ["a", "b", "c"]
  assert $ split (regex "b" noFlags) "" == [""]
  assert $ split (regex "b" noFlags) "abc" == ["a", "c"]

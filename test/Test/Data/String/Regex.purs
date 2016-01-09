module Test.Data.String.Regex (testStringRegex) where

import Prelude
import Data.Maybe
import Control.Monad.Eff.Console (log)
import Data.String.Regex
import Data.Either (isLeft)
import Data.Either.Unsafe (fromRight)
import Test.Assert (assert)

-- | Unsafe version of `regex`.
regex' :: String -> RegexFlags -> Regex
regex' pattern flags = fromRight (regex pattern flags)

testStringRegex = do
  log "regex"
  assert $ test (regex' "^a" noFlags) "abc"
  assert $ not (test (regex' "^b" noFlags) "abc")
  assert $ isLeft (regex "+" noFlags)

  log "match"
  assert $ match (regex' "^abc$" noFlags) "abc" == Just [Just "abc"]

  log "replace"
  assert $ replace (regex' "-" noFlags) "!" "a-b-c" == "a!b-c"

  log "replace'"
  assert $ replace' (regex' "-" noFlags) (\s xs -> "!") "a-b-c" == "a!b-c"

  log "search"
  assert $ search (regex' "b" noFlags) "abc" == Just 1
  assert $ search (regex' "d" noFlags) "abc" == Nothing

  log "split"
  assert $ split (regex' "" noFlags) "" == []
  assert $ split (regex' "" noFlags) "abc" == ["a", "b", "c"]
  assert $ split (regex' "b" noFlags) "" == [""]
  assert $ split (regex' "b" noFlags) "abc" == ["a", "c"]

  log "test"
  -- Ensure that we have referential transparency for calls to 'test'. No
  -- global state should be maintained between these two calls:
  let pattern = regex' "a" (parseFlags "g")
  assert $ test pattern "a"
  assert $ test pattern "a"

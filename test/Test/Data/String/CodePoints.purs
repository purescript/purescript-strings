module Test.Data.String.CodePoints (testStringCodePoints) where

import Prelude (Unit, Ordering(..), (==), ($), discard, negate, not, (/=), (&&), (<))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String.CodePoints

import Test.Assert (ASSERT, assert)

str :: String
str = "a\xDC00\xD800\xD800\x16805\x16A06\&z"

testStringCodePoints :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testStringCodePoints = do
  log "codePointAt"
  assert $ codePointAt (-1) str == Nothing
  assert $ codePointAt 0 str == (codePointFromInt 0x61)
  assert $ codePointAt 1 str == (codePointFromInt 0xDC00)
  assert $ codePointAt 2 str == (codePointFromInt 0xD800)
  assert $ codePointAt 3 str == (codePointFromInt 0xD800)
  assert $ codePointAt 4 str == (codePointFromInt 0x16805)
  assert $ codePointAt 5 str == (codePointFromInt 0x16A06)
  assert $ codePointAt 6 str == (codePointFromInt 0x7A)
  assert $ codePointAt 7 str == Nothing

  log "count"
  assert $ count (\_ -> true) "" == 0
  assert $ count (\_ -> false) str == 0
  assert $ count (\_ -> true) str == 7
  assert $ count (\x -> codePointToInt x < 0xFFFF) str == 4
  assert $ count (\x -> codePointToInt x < 0xDC00) str == 1

  log "drop"
  assert $ drop (-1) str == str
  assert $ drop 0 str == str
  assert $ drop 1 str == "\xDC00\xD800\xD800\x16805\x16A06\&z"
  assert $ drop 2 str == "\xD800\xD800\x16805\x16A06\&z"
  assert $ drop 3 str == "\xD800\x16805\x16A06\&z"
  assert $ drop 4 str == "\x16805\x16A06\&z"
  assert $ drop 5 str == "\x16A06\&z"
  assert $ drop 6 str == "z"
  assert $ drop 7 str == ""
  assert $ drop 8 str == ""

  log "dropWhile"
  assert $ dropWhile (\c -> true) str == ""
  assert $ dropWhile (\c -> false) str == str
  assert $ dropWhile (\c -> codePointToInt c < 0xFFFF) str == "\x16805\x16A06\&z"
  assert $ dropWhile (\c -> codePointToInt c < 0xDC00) str == "\xDC00\xD800\xD800\x16805\x16A06\&z"

  log "indexOf"
  assert $ indexOf (Pattern "") "" == Just 0
  assert $ indexOf (Pattern "") str == Just 0
  assert $ indexOf (Pattern "a") str == Just 0
  assert $ indexOf (Pattern "\xDC00\xD800\xD800") str == Just 1
  assert $ indexOf (Pattern "\xD800") str == Just 2
  assert $ indexOf (Pattern "\xD800\xD800") str == Just 2
  assert $ indexOf (Pattern "\xD800\xD81A") str == Just 3
  assert $ indexOf (Pattern "\xD800\x16805") str == Just 3
  assert $ indexOf (Pattern "\x16805") str == Just 4
  assert $ indexOf (Pattern "\x16A06") str == Just 5
  assert $ indexOf (Pattern "z") str == Just 6
  assert $ indexOf (Pattern "\0") str == Nothing
  assert $ indexOf (Pattern "\xD81A") str == Just 4
  -- TODO: Should this be Nothing? It matches the trail surrogate of a surrogate pair.
  -- It'd be nice if (drop (indexOf pattern str) str) was guaranteed to start with pattern.
  -- If we change this, we'll also need to add a matching contains implementation to the CodePoints module.
  -- I vote we just delete the test. Passing surrogate halves to the CodePoints functions should not be supported.
  assert $ indexOf (Pattern "\xDC05") str == Just 5

--  log "singleton"
--  assert $ singleton 'a' == "a"
--
--  log "takeWhile"
--  assert $ takeWhile (\c -> true) "abc" == "abc"
--  assert $ takeWhile (\c -> false) "abc" == ""
--  assert $ takeWhile (\c -> c /= 'b') "aabbcc" == "aa"
--
--  log "indexOf'"
--  assert $ indexOf' (Pattern "") 0 "" == Just 0
--  assert $ indexOf' (Pattern "") (-1) "ab" == Nothing
--  assert $ indexOf' (Pattern "") 0 "ab" == Just 0
--  assert $ indexOf' (Pattern "") 1 "ab" == Just 1
--  assert $ indexOf' (Pattern "") 2 "ab" == Just 2
--  assert $ indexOf' (Pattern "") 3 "ab" == Nothing
--  assert $ indexOf' (Pattern "bc") 0 "abcd" == Just 1
--  assert $ indexOf' (Pattern "bc") 1 "abcd" == Just 1
--  assert $ indexOf' (Pattern "bc") 2 "abcd" == Nothing
--  assert $ indexOf' (Pattern "cb") 0 "abcd" == Nothing
--
--  log "lastIndexOf"
--  assert $ lastIndexOf (Pattern "") "" == Just 0
--  assert $ lastIndexOf (Pattern "") "abcd" == Just 4
--  assert $ lastIndexOf (Pattern "bc") "abcd" == Just 1
--  assert $ lastIndexOf (Pattern "cb") "abcd" == Nothing
--
--  log "lastIndexOf'"
--  assert $ lastIndexOf' (Pattern "") 0 "" == Just 0
--  assert $ lastIndexOf' (Pattern "") (-1) "ab" == Nothing
--  assert $ lastIndexOf' (Pattern "") 0 "ab" == Just 0
--  assert $ lastIndexOf' (Pattern "") 1 "ab" == Just 1
--  assert $ lastIndexOf' (Pattern "") 2 "ab" == Just 2
--  assert $ lastIndexOf' (Pattern "") 3 "ab" == Nothing
--  assert $ lastIndexOf' (Pattern "bc") 0 "abcd" == Nothing
--  assert $ lastIndexOf' (Pattern "bc") 1 "abcd" == Just 1
--  assert $ lastIndexOf' (Pattern "bc") 2 "abcd" == Just 1
--  assert $ lastIndexOf' (Pattern "cb") 0 "abcd" == Nothing
--
--  log "length"
--  assert $ length "" == 0
--  assert $ length "a" == 1
--  assert $ length "ab" == 2
--
--  log "take"
--  assert $ take 0 "ab" == ""
--  assert $ take 1 "ab" == "a"
--  assert $ take 2 "ab" == "ab"
--  assert $ take 3 "ab" == "ab"
--  assert $ take (-1) "ab" == ""
--
--  log "count"
--  assert $ count (_ == 'a') "" == 0
--  assert $ count (_ == 'a') "ab" == 1
--  assert $ count (_ == 'a') "aaab" == 3
--  assert $ count (_ == 'a') "abaa" == 1
--
--  log "splitAt"
--  let testSplitAt i str res =
--        assert $ case splitAt i str of
--          Nothing ->
--            isNothing res
--          Just { before, after } ->
--            maybe false (\r ->
--              r.before == before && r.after == after) res
--
--  testSplitAt 1 "" Nothing
--  testSplitAt 0 "a" $ Just {before: "", after: "a"}
--  testSplitAt 1 "ab" $ Just {before: "a", after: "b"}
--  testSplitAt 3 "aabcc" $ Just {before: "aab", after: "cc"}
--  testSplitAt (-1) "abc" $ Nothing

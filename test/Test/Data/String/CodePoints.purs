module Test.Data.String.CodePoints (testStringCodePoints) where

import Prelude

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
  assert $ dropWhile (\_ -> true) str == ""
  assert $ dropWhile (\_ -> false) str == str
  assert $ dropWhile (\c -> codePointToInt c < 0xFFFF) str == "\x16805\x16A06\&z"
  assert $ dropWhile (\c -> codePointToInt c < 0xDC00) str == "\xDC00\xD800\xD800\x16805\x16A06\&z"

  log "indexOf"
  assert $ indexOf (Pattern "") "" == Just 0
  assert $ indexOf (Pattern "") str == Just 0
  assert $ indexOf (Pattern str) str == Just 0
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

  log "indexOf'"
  assert $ indexOf' (Pattern "") 0 "" == Just 0
  assert $ indexOf' (Pattern str) 0 str == Just 0
  assert $ indexOf' (Pattern str) 1 str == Nothing
  assert $ indexOf' (Pattern "a") 0 str == Just 0
  assert $ indexOf' (Pattern "a") 1 str == Nothing
  assert $ indexOf' (Pattern "z") 0 str == Just 6
  assert $ indexOf' (Pattern "z") 1 str == Just 6
  assert $ indexOf' (Pattern "z") 2 str == Just 6
  assert $ indexOf' (Pattern "z") 3 str == Just 6
  assert $ indexOf' (Pattern "z") 4 str == Just 6
  assert $ indexOf' (Pattern "z") 5 str == Just 6
  assert $ indexOf' (Pattern "z") 6 str == Just 6
  assert $ indexOf' (Pattern "z") 7 str == Nothing

  log "lastIndexOf"
  assert $ lastIndexOf (Pattern "") "" == Just 0
  assert $ lastIndexOf (Pattern "") str == Just 7
  assert $ lastIndexOf (Pattern str) str == Just 0
  assert $ lastIndexOf (Pattern "a") str == Just 0
  assert $ lastIndexOf (Pattern "\xDC00\xD800\xD800") str == Just 1
  assert $ lastIndexOf (Pattern "\xD800") str == Just 3
  assert $ lastIndexOf (Pattern "\xD800\xD800") str == Just 2
  assert $ lastIndexOf (Pattern "\xD800\xD81A") str == Just 3
  assert $ lastIndexOf (Pattern "\xD800\x16805") str == Just 3
  assert $ lastIndexOf (Pattern "\x16805") str == Just 4
  assert $ lastIndexOf (Pattern "\x16A06") str == Just 5
  assert $ lastIndexOf (Pattern "z") str == Just 6
  assert $ lastIndexOf (Pattern "\0") str == Nothing
  assert $ lastIndexOf (Pattern "\xD81A") str == Just 5

  log "lastIndexOf'"
  assert $ lastIndexOf' (Pattern "") 0 "" == Just 0
  assert $ lastIndexOf' (Pattern str) 0 str == Just 0
  assert $ lastIndexOf' (Pattern str) 1 str == Just 0
  assert $ lastIndexOf' (Pattern "a") 0 str == Just 0
  assert $ lastIndexOf' (Pattern "a") 7 str == Just 0
  assert $ lastIndexOf' (Pattern "z") 0 str == Nothing
  assert $ lastIndexOf' (Pattern "z") 1 str == Nothing
  assert $ lastIndexOf' (Pattern "z") 2 str == Nothing
  assert $ lastIndexOf' (Pattern "z") 3 str == Nothing
  assert $ lastIndexOf' (Pattern "z") 4 str == Nothing
  assert $ lastIndexOf' (Pattern "z") 5 str == Nothing
  assert $ lastIndexOf' (Pattern "z") 6 str == Just 6
  assert $ lastIndexOf' (Pattern "z") 7 str == Just 6
  assert $ lastIndexOf' (Pattern "\xD800") 7 str == Just 3
  assert $ lastIndexOf' (Pattern "\xD800") 6 str == Just 3
  assert $ lastIndexOf' (Pattern "\xD800") 5 str == Just 3
  assert $ lastIndexOf' (Pattern "\xD800") 4 str == Just 3
  assert $ lastIndexOf' (Pattern "\xD800") 3 str == Just 3
  assert $ lastIndexOf' (Pattern "\xD800") 2 str == Just 2
  assert $ lastIndexOf' (Pattern "\xD800") 1 str == Nothing
  assert $ lastIndexOf' (Pattern "\xD800") 0 str == Nothing
  assert $ lastIndexOf' (Pattern "\x16A06") 7 str == Just 5
  assert $ lastIndexOf' (Pattern "\x16A06") 6 str == Just 5
  assert $ lastIndexOf' (Pattern "\x16A06") 5 str == Just 5
  assert $ lastIndexOf' (Pattern "\x16A06") 4 str == Nothing
  assert $ lastIndexOf' (Pattern "\x16A06") 3 str == Nothing

  log "length"
  assert $ length "" == 0
  assert $ length "a" == 1
  assert $ length "ab" == 2
  assert $ length str == 7

  log "singleton"
  assert $ (singleton <$> codePointFromInt 0x30) == Just "0"
  assert $ (singleton <$> codePointFromInt 0x16805) == Just "\x16805"

  log "splitAt"
  let testSplitAt i s res =
        assert $ case splitAt i s of
          Nothing ->
            isNothing res
          Just { before, after } ->
            maybe false (\r ->
              r.before == before && r.after == after) res

  testSplitAt 0 "" $ Just {before: "", after: ""}
  testSplitAt 1 "" Nothing
  testSplitAt 0 "a" $ Just {before: "", after: "a"}
  testSplitAt 1 "ab" $ Just {before: "a", after: "b"}
  testSplitAt 3 "aabcc" $ Just {before: "aab", after: "cc"}
  testSplitAt (-1) "abc" $ Nothing
  testSplitAt 0 str $ Just {before: "", after: str}
  testSplitAt 1 str $ Just {before: "a", after: "\xDC00\xD800\xD800\x16805\x16A06\&z"}
  testSplitAt 2 str $ Just {before: "a\xDC00", after: "\xD800\xD800\x16805\x16A06\&z"}
  testSplitAt 3 str $ Just {before: "a\xDC00\xD800", after: "\xD800\x16805\x16A06\&z"}
  testSplitAt 4 str $ Just {before: "a\xDC00\xD800\xD800", after: "\x16805\x16A06\&z"}
  testSplitAt 5 str $ Just {before: "a\xDC00\xD800\xD800\x16805", after: "\x16A06\&z"}
  testSplitAt 6 str $ Just {before: "a\xDC00\xD800\xD800\x16805\x16A06", after: "z"}
  testSplitAt 7 str $ Just {before: str, after: ""}
  testSplitAt 8 str $ Nothing

  log "take"
  assert $ take (-1) str == ""
  assert $ take 0 str == ""
  assert $ take 1 str == "a"
  assert $ take 2 str == "a\xDC00"
  assert $ take 3 str == "a\xDC00\xD800"
  assert $ take 4 str == "a\xDC00\xD800\xD800"
  assert $ take 5 str == "a\xDC00\xD800\xD800\x16805"
  assert $ take 6 str == "a\xDC00\xD800\xD800\x16805\x16A06"
  assert $ take 7 str == str
  assert $ take 8 str == str

  log "takeWhile"
  assert $ takeWhile (\_ -> true) str == str
  assert $ takeWhile (\_ -> false) str == ""
  assert $ takeWhile (\c -> codePointToInt c < 0xFFFF) str == "a\xDC00\xD800\xD800"
  assert $ takeWhile (\c -> codePointToInt c < 0xDC00) str == "a"

  log "uncons"
  let testUncons s res =
        assert $ case uncons s of
          Nothing ->
            isNothing res
          Just { head, tail } ->
            maybe false (\r ->
              r.head == codePointToInt head && r.tail == tail) res

  testUncons str $ Just {head: 0x61, tail:  "\xDC00\xD800\xD800\x16805\x16A06\&z"}
  testUncons (drop 1 str) $ Just {head: 0xDC00, tail: "\xD800\xD800\x16805\x16A06\&z"}
  testUncons (drop 2 str) $ Just {head: 0xD800, tail: "\xD800\x16805\x16A06\&z"}
  testUncons (drop 3 str) $ Just {head: 0xD800, tail: "\x16805\x16A06\&z"}
  testUncons (drop 4 str) $ Just {head: 0x16805, tail: "\x16A06\&z"}
  testUncons (drop 5 str) $ Just {head: 0x16A06, tail: "z"}
  testUncons (drop 6 str) $ Just {head: 0x7A, tail: ""}
  testUncons "" Nothing

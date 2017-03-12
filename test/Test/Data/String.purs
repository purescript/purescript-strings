module Test.Data.String (testString) where

import Prelude (Unit, Ordering(..), (==), ($), discard, negate, not, (/=), (&&))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(..), isNothing)
import Data.String

import Test.Assert (ASSERT, assert)

testString :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testString = do
  log "charAt"
  assert $ charAt 0 "" == Nothing
  assert $ charAt 0 "a" == Just 'a'
  assert $ charAt 1 "a" == Nothing
  assert $ charAt 0 "ab" == Just 'a'
  assert $ charAt 1 "ab" == Just 'b'
  assert $ charAt 2 "ab" == Nothing

  log "singleton"
  assert $ singleton 'a' == "a"

  log "charCodeAt"
  assert $ charCodeAt 0 "" == Nothing
  assert $ charCodeAt 0 "a" == Just 97
  assert $ charCodeAt 1 "a" == Nothing
  assert $ charCodeAt 0 "ab" == Just 97
  assert $ charCodeAt 1 "ab" == Just 98
  assert $ charCodeAt 2 "ab" == Nothing

  log "toChar"
  assert $ toChar "" == Nothing
  assert $ toChar "a" == Just 'a'
  assert $ toChar "ab" == Nothing

  log "null"
  assert $ null ""
  assert $ not (null "a")

  log "uncons"
  assert $ isNothing (uncons "")
  assert $ case uncons "a" of
                Nothing -> false
                Just m -> m.head == 'a' && m.tail == ""
  assert $ case uncons "ab" of
                Nothing -> false
                Just m -> m.head == 'a' && m.tail == "b"

  log "takeWhile"
  assert $ takeWhile (\c -> true) "abc" == "abc"
  assert $ takeWhile (\c -> false) "abc" == ""
  assert $ takeWhile (\c -> c /= 'b') "aabbcc" == "aa"

  log "dropWhile"
  assert $ dropWhile (\c -> true) "abc" == ""
  assert $ dropWhile (\c -> false) "abc" == "abc"
  assert $ dropWhile (\c -> c /= 'b') "aabbcc" == "bbcc"

  log "stripPrefix"
  assert $ stripPrefix (Pattern "") "" == Just ""
  assert $ stripPrefix (Pattern "") "abc" == Just "abc"
  assert $ stripPrefix (Pattern "a") "abc" == Just "bc"
  assert $ stripPrefix (Pattern "!") "abc" == Nothing
  assert $ stripPrefix (Pattern "!") "" == Nothing

  log "fromCharArray"
  assert $ fromCharArray [] == ""
  assert $ fromCharArray ['a', 'b'] == "ab"

  log "contains"
  assert $ contains (Pattern "") ""
  assert $ contains (Pattern "") "abcd"
  assert $ contains (Pattern "bc") "abcd"
  assert $ not (contains (Pattern "cb") "abcd")

  log "indexOf"
  assert $ indexOf (Pattern "") "" == Just 0
  assert $ indexOf (Pattern "") "abcd" == Just 0
  assert $ indexOf (Pattern "bc") "abcd" == Just 1
  assert $ indexOf (Pattern "cb") "abcd" == Nothing

  log "indexOf'"
  assert $ indexOf' (Pattern "") 0 "" == Just 0
  assert $ indexOf' (Pattern "") (-1) "ab" == Nothing
  assert $ indexOf' (Pattern "") 0 "ab" == Just 0
  assert $ indexOf' (Pattern "") 1 "ab" == Just 1
  assert $ indexOf' (Pattern "") 2 "ab" == Just 2
  assert $ indexOf' (Pattern "") 3 "ab" == Nothing
  assert $ indexOf' (Pattern "bc") 0 "abcd" == Just 1
  assert $ indexOf' (Pattern "bc") 1 "abcd" == Just 1
  assert $ indexOf' (Pattern "bc") 2 "abcd" == Nothing
  assert $ indexOf' (Pattern "cb") 0 "abcd" == Nothing

  log "lastIndexOf"
  assert $ lastIndexOf (Pattern "") "" == Just 0
  assert $ lastIndexOf (Pattern "") "abcd" == Just 4
  assert $ lastIndexOf (Pattern "bc") "abcd" == Just 1
  assert $ lastIndexOf (Pattern "cb") "abcd" == Nothing

  log "lastIndexOf'"
  assert $ lastIndexOf' (Pattern "") 0 "" == Just 0
  assert $ lastIndexOf' (Pattern "") (-1) "ab" == Nothing
  assert $ lastIndexOf' (Pattern "") 0 "ab" == Just 0
  assert $ lastIndexOf' (Pattern "") 1 "ab" == Just 1
  assert $ lastIndexOf' (Pattern "") 2 "ab" == Just 2
  assert $ lastIndexOf' (Pattern "") 3 "ab" == Nothing
  assert $ lastIndexOf' (Pattern "bc") 0 "abcd" == Nothing
  assert $ lastIndexOf' (Pattern "bc") 1 "abcd" == Just 1
  assert $ lastIndexOf' (Pattern "bc") 2 "abcd" == Just 1
  assert $ lastIndexOf' (Pattern "cb") 0 "abcd" == Nothing

  log "length"
  assert $ length "" == 0
  assert $ length "a" == 1
  assert $ length "ab" == 2

  log "localeCompare"
  assert $ localeCompare "" "" == EQ
  assert $ localeCompare "a" "a" == EQ
  assert $ localeCompare "a" "b" == LT
  assert $ localeCompare "b" "a" == GT

  log "replace"
  assert $ replace (Pattern "b") (Replacement "") "abc" == "ac"
  assert $ replace (Pattern "b") (Replacement "!") "abc" == "a!c"
  assert $ replace (Pattern "d") (Replacement "!") "abc" == "abc"

  log "replaceAll"
  assert $ replaceAll (Pattern "b") (Replacement "") "abbbbbc" == "ac"
  assert $ replaceAll (Pattern "[b]") (Replacement "!") "a[b]c" == "a!c"

  log "take"
  assert $ take 0 "ab" == ""
  assert $ take 1 "ab" == "a"
  assert $ take 2 "ab" == "ab"
  assert $ take 3 "ab" == "ab"
  assert $ take (-1) "ab" == ""

  log "drop"
  assert $ drop 0 "ab" == "ab"
  assert $ drop 1 "ab" == "b"
  assert $ drop 2 "ab" == ""
  assert $ drop 3 "ab" == ""
  assert $ drop (-1) "ab" == "ab"

  log "count"
  assert $ count (_ == 'a') "" == 0
  assert $ count (_ == 'a') "ab" == 1
  assert $ count (_ == 'a') "aaab" == 3
  assert $ count (_ == 'a') "abaa" == 1

  log "split"
  assert $ split (Pattern "") "" == []
  assert $ split (Pattern "") "a" == ["a"]
  assert $ split (Pattern "") "ab" == ["a", "b"]
  assert $ split (Pattern "b") "aabcc" == ["aa", "cc"]
  assert $ split (Pattern "d") "abc" == ["abc"]

  log "splitAt"
  assert $ splitAt 1 "" == Nothing
  assert $ splitAt 0 "a" == Just ["", "a"]
  assert $ splitAt 1 "ab" == Just ["a", "b"]
  assert $ splitAt 3 "aabcc" == Just ["aab", "cc"]
  assert $ splitAt (-1) "abc" == Nothing

  log "toCharArray"
  assert $ toCharArray "" == []
  assert $ toCharArray "a" == ['a']
  assert $ toCharArray "ab" == ['a', 'b']

  log "toLower"
  assert $ toLower "bAtMaN" == "batman"

  log "toUpper"
  assert $ toUpper "bAtMaN" == "BATMAN"

  log "trim"
  assert $ trim "  abc  " == "abc"

  log "joinWith"
  assert $ joinWith "" [] == ""
  assert $ joinWith "" ["a", "b"] == "ab"
  assert $ joinWith "--" ["a", "b", "c"] == "a--b--c"

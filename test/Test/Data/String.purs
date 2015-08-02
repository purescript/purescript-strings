module Test.Data.String (testString) where

import Prelude
import Data.Maybe
import Control.Monad.Eff.Console (log)
import Data.String
import Test.Assert (assert)

testString = do
  log "charAt"
  assert $ charAt 0 "" == Nothing
  assert $ charAt 0 "a" == Just 'a'
  assert $ charAt 1 "a" == Nothing
  assert $ charAt 0 "ab" == Just 'a'
  assert $ charAt 1 "ab" == Just 'b'
  assert $ charAt 2 "ab" == Nothing

  log "fromChar"
  assert $ fromChar 'a' == "a"

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
  assert $ stripPrefix "" "" == Just ""
  assert $ stripPrefix "" "abc" == Just "abc"
  assert $ stripPrefix "a" "abc" == Just "bc"
  assert $ stripPrefix "!" "abc" == Nothing
  assert $ stripPrefix "!" "" == Nothing

  log "fromCharArray"
  assert $ fromCharArray [] == ""
  assert $ fromCharArray ['a', 'b'] == "ab"

  log "contains"
  assert $ contains "" ""
  assert $ contains "" "abcd"
  assert $ contains "bc" "abcd"
  assert $ not (contains "cb" "abcd")

  log "indexOf"
  assert $ indexOf "" "" == Just 0
  assert $ indexOf "" "abcd" == Just 0
  assert $ indexOf "bc" "abcd" == Just 1
  assert $ indexOf "cb" "abcd" == Nothing

  log "indexOf'"
  assert $ indexOf' "" 0 "" == Just 0
  assert $ indexOf' "" (-1) "ab" == Nothing
  assert $ indexOf' "" 0 "ab" == Just 0
  assert $ indexOf' "" 1 "ab" == Just 1
  assert $ indexOf' "" 2 "ab" == Just 2
  assert $ indexOf' "" 3 "ab" == Nothing
  assert $ indexOf' "bc" 0 "abcd" == Just 1
  assert $ indexOf' "bc" 1 "abcd" == Just 1
  assert $ indexOf' "bc" 2 "abcd" == Nothing
  assert $ indexOf' "cb" 0 "abcd" == Nothing

  log "lastIndexOf"
  assert $ lastIndexOf "" "" == Just 0
  assert $ lastIndexOf "" "abcd" == Just 4
  assert $ lastIndexOf "bc" "abcd" == Just 1
  assert $ lastIndexOf "cb" "abcd" == Nothing

  log "lastIndexOf'"
  assert $ lastIndexOf' "" 0 "" == Just 0
  assert $ lastIndexOf' "" (-1) "ab" == Nothing
  assert $ lastIndexOf' "" 0 "ab" == Just 0
  assert $ lastIndexOf' "" 1 "ab" == Just 1
  assert $ lastIndexOf' "" 2 "ab" == Just 2
  assert $ lastIndexOf' "" 3 "ab" == Nothing
  assert $ lastIndexOf' "bc" 0 "abcd" == Nothing
  assert $ lastIndexOf' "bc" 1 "abcd" == Just 1
  assert $ lastIndexOf' "bc" 2 "abcd" == Just 1
  assert $ lastIndexOf' "cb" 0 "abcd" == Nothing

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
  assert $ replace "b" "" "abc" == "ac"
  assert $ replace "b" "!" "abc" == "a!c"
  assert $ replace "d" "!" "abc" == "abc"

  log "take"
  assert $ take 0 "ab" == ""
  assert $ take 1 "ab" == "a"
  assert $ take 2 "ab" == "ab"
  assert $ take 3 "ab" == "ab"

  log "drop"
  assert $ drop 0 "ab" == "ab"
  assert $ drop 1 "ab" == "b"
  assert $ drop 2 "ab" == ""
  assert $ drop 3 "ab" == ""

  log "count"
  assert $ count (\c -> true) "" == 0
  assert $ count (\c -> true) "ab" == 2
  assert $ count (\c -> false) "ab" == 0
  assert $ count (\c -> c == 'a') "aabbcc" == 2
  assert $ count (\c -> c == 'b') "aabbcc" == 0
  assert $ count (\c -> c /= 'a') "aabbcc" == 0
  assert $ count (\c -> c /= 'b') "aabbcc" == 2

  log "split"
  assert $ split "" "" == []
  assert $ split "" "a" == ["a"]
  assert $ split "" "ab" == ["a", "b"]
  assert $ split "b" "aabcc" == ["aa", "cc"]
  assert $ split "d" "abc" == ["abc"]

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


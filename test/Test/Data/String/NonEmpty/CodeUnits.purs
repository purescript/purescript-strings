module Test.Data.String.NonEmpty.CodeUnits (testNonEmptyStringCodeUnits) where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.NonEmpty (Pattern(..), nes)
import Data.String.NonEmpty.CodeUnits as NESCU
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assertEqual)
import Type.Proxy (Proxy(..))

testNonEmptyStringCodeUnits :: Effect Unit
testNonEmptyStringCodeUnits = do

  log "fromCharArray"
  assertEqual
    { actual: NESCU.fromCharArray []
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.fromCharArray ['a', 'b']
    , expected: Just (nes @"ab")
    }

  log "fromNonEmptyCharArray"
  assertEqual
    { actual: NESCU.fromNonEmptyCharArray (NEA.singleton 'b')
    , expected: NESCU.singleton 'b'
    }

  log "singleton"
  assertEqual
    { actual: NESCU.singleton 'a'
    , expected: nes @"a"
    }

  log "cons"
  assertEqual
    { actual: NESCU.cons 'a' "bc"
    , expected: nes @"abc"
    }
  assertEqual
    { actual: NESCU.cons 'a' ""
    , expected: nes @"a"
    }

  log "snoc"
  assertEqual
    { actual: NESCU.snoc 'c' "ab"
    , expected: nes @"abc"
    }
  assertEqual
    { actual: NESCU.snoc 'a' ""
    , expected: nes @"a"
    }

  log "fromFoldable1"
  assertEqual
    { actual: NESCU.fromFoldable1 (nea ['a'])
    , expected: nes @"a"
    }
  assertEqual
    { actual: NESCU.fromFoldable1 (nea ['a', 'b', 'c'])
    , expected: nes @"abc"
    }

  log "charAt"
  assertEqual
    { actual: NESCU.charAt 0 (nes @"a")
    , expected: Just 'a'
    }
  assertEqual
    { actual: NESCU.charAt 1 (nes @"a")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.charAt 0 (nes @"ab")
    , expected: Just 'a'
    }
  assertEqual
    { actual: NESCU.charAt 1 (nes @"ab")
    , expected: Just 'b'
    }
  assertEqual
    { actual: NESCU.charAt 2 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.charAt 2 (nes @"Hello")
    , expected: Just 'l'
    }
  assertEqual
    { actual: NESCU.charAt 10 (nes @"Hello")
    , expected: Nothing
    }

  log "charCodeAt"
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 0 (nes @"a")
    , expected: Just 97
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 1 (nes @"a")
    , expected: Nothing
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 0 (nes @"ab")
    , expected: Just 97
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 1 (nes @"ab")
    , expected: Just 98
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 2 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 2 (nes @"5 €")
    , expected: Just 0x20AC
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 10 (nes @"5 €")
    , expected: Nothing
    }

  log "toChar"
  assertEqual
    { actual: NESCU.toChar (nes @"a")
    , expected: Just 'a'
    }
  assertEqual
    { actual: NESCU.toChar (nes @"ab")
    , expected: Nothing
    }

  log "toCharArray"
  assertEqual
    { actual: NESCU.toCharArray (nes @"a")
    , expected: ['a']
    }
  assertEqual
    { actual: NESCU.toCharArray (nes @"ab")
    , expected: ['a','b']
    }
  assertEqual
    { actual: NESCU.toCharArray (nes @"Hello☺\n")
    , expected: ['H','e','l','l','o','☺','\n']
    }

  log "toNonEmptyCharArray"
  assertEqual
    { actual: NESCU.toNonEmptyCharArray (nes @"ab")
    , expected: nea ['a', 'b']
    }

  log "uncons"
  assertEqual
    { actual: NESCU.uncons (nes @"a")
    , expected: { head: 'a', tail: Nothing }
    }
  assertEqual
    { actual: NESCU.uncons (nes @"Hello World")
    , expected: { head: 'H', tail: Just (nes @"ello World") }
    }

  log "takeWhile"
  assertEqual
    { actual: NESCU.takeWhile (\_ -> true) (nes @"abc")
    , expected: Just (nes @"abc")
    }
  assertEqual
    { actual: NESCU.takeWhile (\_ -> false) (nes @"abc")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.takeWhile (\c -> c /= 'b') (nes @"aabbcc")
    , expected: Just (nes @"aa")
    }
  assertEqual
    { actual: NESCU.takeWhile (_ /= ':') (nes @"http://purescript.org")
    , expected: Just (nes @"http")
    }
  assertEqual
    { actual: NESCU.takeWhile (_ == 'a') (nes @"xyz")
    , expected: Nothing
    }

  log "dropWhile"
  assertEqual
    { actual: NESCU.dropWhile (\_ -> true) (nes @"abc")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.dropWhile (\_ -> false) (nes @"abc")
    , expected: Just (nes @"abc")
    }
  assertEqual
    { actual: NESCU.dropWhile (\c -> c /= 'b') (nes @"aabbcc")
    , expected: Just (nes @"bbcc")
    }
  assertEqual
    { actual: NESCU.dropWhile (_ /= '.') (nes @"Test.purs")
    , expected: Just (nes @".purs")
    }

  log "indexOf"
  assertEqual
    { actual: NESCU.indexOf (Pattern "") (nes @"abcd")
    , expected: Just 0
    }
  assertEqual
    { actual: NESCU.indexOf (Pattern "bc") (nes @"abcd")
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.indexOf (Pattern "cb") (nes @"abcd")
    , expected: Nothing
    }

  log "indexOf'"
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") (-1) (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") 0 (nes @"ab")
    , expected: Just 0
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") 1 (nes @"ab")
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") 2 (nes @"ab")
    , expected: Just 2
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") 3 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "bc") 0 (nes @"abcd")
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "bc") 1 (nes @"abcd")
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "bc") 2 (nes @"abcd")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "cb") 0 (nes @"abcd")
    , expected: Nothing
    }

  log "lastIndexOf"
  assertEqual
    { actual: NESCU.lastIndexOf (Pattern "") (nes @"abcd")
    , expected: Just 4
    }
  assertEqual
    { actual: NESCU.lastIndexOf (Pattern "bc") (nes @"abcd")
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.lastIndexOf (Pattern "cb") (nes @"abcd")
    , expected: Nothing
    }

  log "lastIndexOf'"
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") (-1) (nes @"ab")
    , expected: Just 0
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") 0 (nes @"ab")
    , expected: Just 0
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") 1 (nes @"ab")
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") 2 (nes @"ab")
    , expected: Just 2
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") 3 (nes @"ab")
    , expected: Just 2
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "bc") 0 (nes @"abcd")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "bc") 1 (nes @"abcd")
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "bc") 2 (nes @"abcd")
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "cb") 0 (nes @"abcd")
    , expected: Nothing
    }

  log "length"
  assertEqual
    { actual: NESCU.length (nes @"a")
    , expected: 1
    }
  assertEqual
    { actual: NESCU.length (nes @"ab")
    , expected: 2
    }

  log "take"
  assertEqual
    { actual: NESCU.take 0 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.take 1 (nes @"ab")
    , expected: Just (nes @"a")
    }
  assertEqual
    { actual: NESCU.take 2 (nes @"ab")
    , expected: Just (nes @"ab")
    }
  assertEqual
    { actual: NESCU.take 3 (nes @"ab")
    , expected: Just (nes @"ab")
    }
  assertEqual
    { actual: NESCU.take (-1) (nes @"ab")
    , expected: Nothing
    }

  log "takeRight"
  assertEqual
    { actual: NESCU.takeRight 0 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.takeRight 1 (nes @"ab")
    , expected: Just (nes @"b")
    }
  assertEqual
    { actual: NESCU.takeRight 2 (nes @"ab")
    , expected: Just (nes @"ab")
    }
  assertEqual
    { actual: NESCU.takeRight 3 (nes @"ab")
    , expected: Just (nes @"ab")
    }
  assertEqual
    { actual: NESCU.takeRight (-1) (nes @"ab")
    , expected: Nothing
    }

  log "drop"
  assertEqual
    { actual: NESCU.drop 0 (nes @"ab")
    , expected: Just (nes @"ab")
    }
  assertEqual
    { actual: NESCU.drop 1 (nes @"ab")
    , expected: Just (nes @"b")
    }
  assertEqual
    { actual: NESCU.drop 2 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.drop 3 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.drop (-1) (nes @"ab")
    , expected: Just (nes @"ab")
    }

  log "dropRight"
  assertEqual
    { actual: NESCU.dropRight 0 (nes @"ab")
    , expected: Just (nes @"ab")
    }
  assertEqual
    { actual: NESCU.dropRight 1 (nes @"ab")
    , expected: Just (nes @"a")
    }
  assertEqual
    { actual: NESCU.dropRight 2 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.dropRight 3 (nes @"ab")
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.dropRight (-1) (nes @"ab")
    , expected: Just (nes @"ab")
    }

  log "countPrefix"
  assertEqual
    { actual: NESCU.countPrefix (_ == 'a') (nes @"ab")
    , expected: 1
    }
  assertEqual
    { actual: NESCU.countPrefix (_ == 'a') (nes @"aaab")
    , expected: 3
    }
  assertEqual
    { actual: NESCU.countPrefix (_ == 'a') (nes @"abaa")
    , expected: 1
    }
  assertEqual
    { actual: NESCU.countPrefix (_ == 'c') (nes @"abaa")
    , expected: 0
    }

  log "splitAt"
  assertEqual
    { actual: NESCU.splitAt 0 (nes @"a")
    , expected: { before: Nothing, after: Just (nes @"a") }
    }
  assertEqual
    { actual: NESCU.splitAt 1 (nes @"ab")
    , expected: { before: Just (nes @"a"), after: Just (nes @"b") }
    }
  assertEqual
    { actual: NESCU.splitAt 3 (nes @"aabcc")
    , expected: { before: Just (nes @"aab"), after: Just (nes @"cc") }
    }
  assertEqual
    { actual: NESCU.splitAt (-1) (nes @"abc")
    , expected: { before: Nothing, after: Just (nes @"abc") }
    }

nea :: Array ~> NEA.NonEmptyArray
nea = unsafePartial fromJust <<< NEA.fromArray

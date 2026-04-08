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
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }

  log "fromNonEmptyCharArray"
  assertEqual
    { actual: NESCU.fromNonEmptyCharArray (NEA.singleton 'b')
    , expected: NESCU.singleton 'b'
    }

  log "singleton"
  assertEqual
    { actual: NESCU.singleton 'a'
    , expected: nes (Proxy :: Proxy "a")
    }

  log "cons"
  assertEqual
    { actual: NESCU.cons 'a' "bc"
    , expected: nes (Proxy :: Proxy "abc")
    }
  assertEqual
    { actual: NESCU.cons 'a' ""
    , expected: nes (Proxy :: Proxy "a")
    }

  log "snoc"
  assertEqual
    { actual: NESCU.snoc 'c' "ab"
    , expected: nes (Proxy :: Proxy "abc")
    }
  assertEqual
    { actual: NESCU.snoc 'a' ""
    , expected: nes (Proxy :: Proxy "a")
    }

  log "fromFoldable1"
  assertEqual
    { actual: NESCU.fromFoldable1 (nea ['a'])
    , expected: nes (Proxy :: Proxy "a")
    }
  assertEqual
    { actual: NESCU.fromFoldable1 (nea ['a', 'b', 'c'])
    , expected: nes (Proxy :: Proxy "abc")
    }

  log "charAt"
  assertEqual
    { actual: NESCU.charAt 0 (nes (Proxy :: Proxy "a"))
    , expected: Just 'a'
    }
  assertEqual
    { actual: NESCU.charAt 1 (nes (Proxy :: Proxy "a"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.charAt 0 (nes (Proxy :: Proxy "ab"))
    , expected: Just 'a'
    }
  assertEqual
    { actual: NESCU.charAt 1 (nes (Proxy :: Proxy "ab"))
    , expected: Just 'b'
    }
  assertEqual
    { actual: NESCU.charAt 2 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.charAt 2 (nes (Proxy :: Proxy "Hello"))
    , expected: Just 'l'
    }
  assertEqual
    { actual: NESCU.charAt 10 (nes (Proxy :: Proxy "Hello"))
    , expected: Nothing
    }

  log "charCodeAt"
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 0 (nes (Proxy :: Proxy "a"))
    , expected: Just 97
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 1 (nes (Proxy :: Proxy "a"))
    , expected: Nothing
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 0 (nes (Proxy :: Proxy "ab"))
    , expected: Just 97
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 1 (nes (Proxy :: Proxy "ab"))
    , expected: Just 98
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 2 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 2 (nes (Proxy :: Proxy "5 €"))
    , expected: Just 0x20AC
    }
  assertEqual
    { actual: fromEnum <$> NESCU.charAt 10 (nes (Proxy :: Proxy "5 €"))
    , expected: Nothing
    }

  log "toChar"
  assertEqual
    { actual: NESCU.toChar (nes (Proxy :: Proxy "a"))
    , expected: Just 'a'
    }
  assertEqual
    { actual: NESCU.toChar (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }

  log "toCharArray"
  assertEqual
    { actual: NESCU.toCharArray (nes (Proxy :: Proxy "a"))
    , expected: ['a']
    }
  assertEqual
    { actual: NESCU.toCharArray (nes (Proxy :: Proxy "ab"))
    , expected: ['a', 'b']
    }
  assertEqual
    { actual: NESCU.toCharArray (nes (Proxy :: Proxy "Hello☺\n"))
    , expected: ['H','e','l','l','o','☺','\n']
    }

  log "toNonEmptyCharArray"
  assertEqual
    { actual: NESCU.toNonEmptyCharArray (nes (Proxy :: Proxy "ab"))
    , expected: nea ['a', 'b']
    }

  log "uncons"
  assertEqual
    { actual: NESCU.uncons (nes (Proxy :: Proxy "a"))
    , expected: { head: 'a', tail: Nothing }
    }
  assertEqual
    { actual: NESCU.uncons (nes (Proxy :: Proxy "Hello World"))
    , expected: { head: 'H', tail: Just (nes (Proxy :: Proxy "ello World")) }
    }

  log "takeWhile"
  assertEqual
    { actual: NESCU.takeWhile (\_ -> true) (nes (Proxy :: Proxy "abc"))
    , expected: Just (nes (Proxy :: Proxy "abc"))
    }
  assertEqual
    { actual: NESCU.takeWhile (\_ -> false) (nes (Proxy :: Proxy "abc"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.takeWhile (\c -> c /= 'b') (nes (Proxy :: Proxy "aabbcc"))
    , expected: Just (nes (Proxy :: Proxy "aa"))
    }
  assertEqual
    { actual: NESCU.takeWhile (_ /= ':') (nes (Proxy :: Proxy "http://purescript.org"))
    , expected: Just (nes (Proxy :: Proxy "http"))
    }
  assertEqual
    { actual: NESCU.takeWhile (_ == 'a') (nes (Proxy :: Proxy "xyz"))
    , expected: Nothing
    }

  log "dropWhile"
  assertEqual
    { actual: NESCU.dropWhile (\_ -> true) (nes (Proxy :: Proxy "abc"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.dropWhile (\_ -> false) (nes (Proxy :: Proxy "abc"))
    , expected: Just (nes (Proxy :: Proxy "abc"))
    }
  assertEqual
    { actual: NESCU.dropWhile (\c -> c /= 'b') (nes (Proxy :: Proxy "aabbcc"))
    , expected: Just (nes (Proxy :: Proxy "bbcc"))
    }
  assertEqual
    { actual: NESCU.dropWhile (_ /= '.') (nes (Proxy :: Proxy "Test.purs"))
    , expected: Just (nes (Proxy :: Proxy ".purs"))
    }

  log "indexOf"
  assertEqual
    { actual: NESCU.indexOf (Pattern "") (nes (Proxy :: Proxy "abcd"))
    , expected: Just 0
    }
  assertEqual
    { actual: NESCU.indexOf (Pattern "bc") (nes (Proxy :: Proxy "abcd"))
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.indexOf (Pattern "cb") (nes (Proxy :: Proxy "abcd"))
    , expected: Nothing
    }

  log "indexOf'"
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") (-1) (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") 0 (nes (Proxy :: Proxy "ab"))
    , expected: Just 0
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") 1 (nes (Proxy :: Proxy "ab"))
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") 2 (nes (Proxy :: Proxy "ab"))
    , expected: Just 2
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "") 3 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "bc") 0 (nes (Proxy :: Proxy "abcd"))
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "bc") 1 (nes (Proxy :: Proxy "abcd"))
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "bc") 2 (nes (Proxy :: Proxy "abcd"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.indexOf' (Pattern "cb") 0 (nes (Proxy :: Proxy "abcd"))
    , expected: Nothing
    }

  log "lastIndexOf"
  assertEqual
    { actual: NESCU.lastIndexOf (Pattern "") (nes (Proxy :: Proxy "abcd"))
    , expected: Just 4
    }
  assertEqual
    { actual: NESCU.lastIndexOf (Pattern "bc") (nes (Proxy :: Proxy "abcd"))
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.lastIndexOf (Pattern "cb") (nes (Proxy :: Proxy "abcd"))
    , expected: Nothing
    }

  log "lastIndexOf'"
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") (-1) (nes (Proxy :: Proxy "ab"))
    , expected: Just 0
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") 0 (nes (Proxy :: Proxy "ab"))
    , expected: Just 0
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") 1 (nes (Proxy :: Proxy "ab"))
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") 2 (nes (Proxy :: Proxy "ab"))
    , expected: Just 2
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "") 3 (nes (Proxy :: Proxy "ab"))
    , expected: Just 2
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "bc") 0 (nes (Proxy :: Proxy "abcd"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "bc") 1 (nes (Proxy :: Proxy "abcd"))
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "bc") 2 (nes (Proxy :: Proxy "abcd"))
    , expected: Just 1
    }
  assertEqual
    { actual: NESCU.lastIndexOf' (Pattern "cb") 0 (nes (Proxy :: Proxy "abcd"))
    , expected: Nothing
    }

  log "length"
  assertEqual
    { actual: NESCU.length (nes (Proxy :: Proxy "a"))
    , expected: 1
    }
  assertEqual
    { actual: NESCU.length (nes (Proxy :: Proxy "ab"))
    , expected: 2
    }

  log "take"
  assertEqual
    { actual: NESCU.take 0 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.take 1 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "a"))
    }
  assertEqual
    { actual: NESCU.take 2 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }
  assertEqual
    { actual: NESCU.take 3 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }
  assertEqual
    { actual: NESCU.take (-1) (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }

  log "takeRight"
  assertEqual
    { actual: NESCU.takeRight 0 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.takeRight 1 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "b"))
    }
  assertEqual
    { actual: NESCU.takeRight 2 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }
  assertEqual
    { actual: NESCU.takeRight 3 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }
  assertEqual
    { actual: NESCU.takeRight (-1) (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }

  log "drop"
  assertEqual
    { actual: NESCU.drop 0 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }
  assertEqual
    { actual: NESCU.drop 1 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "b"))
    }
  assertEqual
    { actual: NESCU.drop 2 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.drop 3 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.drop (-1) (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }

  log "dropRight"
  assertEqual
    { actual: NESCU.dropRight 0 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }
  assertEqual
    { actual: NESCU.dropRight 1 (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "a"))
    }
  assertEqual
    { actual: NESCU.dropRight 2 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.dropRight 3 (nes (Proxy :: Proxy "ab"))
    , expected: Nothing
    }
  assertEqual
    { actual: NESCU.dropRight (-1) (nes (Proxy :: Proxy "ab"))
    , expected: Just (nes (Proxy :: Proxy "ab"))
    }

  log "countPrefix"
  assertEqual
    { actual: NESCU.countPrefix (_ == 'a') (nes (Proxy :: Proxy "ab"))
    , expected: 1
    }
  assertEqual
    { actual: NESCU.countPrefix (_ == 'a') (nes (Proxy :: Proxy "aaab"))
    , expected: 3
    }
  assertEqual
    { actual: NESCU.countPrefix (_ == 'a') (nes (Proxy :: Proxy "abaa"))
    , expected: 1
    }
  assertEqual
    { actual: NESCU.countPrefix (_ == 'c') (nes (Proxy :: Proxy "abaa"))
    , expected: 0
    }

  log "splitAt"
  assertEqual
    { actual: NESCU.splitAt 0 (nes (Proxy :: Proxy "a"))
    , expected: { before: Nothing, after: Just (nes (Proxy :: Proxy "a")) }
    }
  assertEqual
    { actual: NESCU.splitAt 1 (nes (Proxy :: Proxy "ab"))
    , expected: { before: Just (nes (Proxy :: Proxy "a")), after: Just (nes (Proxy :: Proxy "b")) }
    }
  assertEqual
    { actual: NESCU.splitAt 3 (nes (Proxy :: Proxy "aabcc"))
    , expected: { before: Just (nes (Proxy :: Proxy "aab")), after: Just (nes (Proxy :: Proxy "cc")) }
    }
  assertEqual
    { actual: NESCU.splitAt (-1) (nes (Proxy :: Proxy "abc"))
    , expected: { before: Nothing, after: Just (nes (Proxy :: Proxy "abc")) }
    }

nea :: Array ~> NEA.NonEmptyArray
nea = unsafePartial fromJust <<< NEA.fromArray

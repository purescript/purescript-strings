module Test.Data.String.NonEmpty (testNonEmptyString) where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromJust)
import Data.String.NonEmpty (Pattern(..), nes)
import Data.String.NonEmpty as NES
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual)

testNonEmptyString :: Effect Unit
testNonEmptyString = do

  log "fromString"
  assertEqual
    { actual: NES.fromString ""
    , expected: Nothing
    }
  assertEqual
    { actual: NES.fromString "hello"
    , expected: Just (nes @"hello")
    }

  log "toString"
  assertEqual
    { actual: (NES.toString <$> NES.fromString "hello")
    , expected: Just "hello"
    }

  log "appendString"
  assertEqual
    { actual: NES.appendString (nes @"Hello") " world"
    , expected: nes @"Hello world"
    }
  assertEqual
    { actual: NES.appendString (nes @"Hello") ""
    , expected: nes @"Hello"
    }

  log "prependString"
  assertEqual
    { actual: NES.prependString "be" (nes @"fore")
    , expected: nes @"before"
    }
  assertEqual
    { actual: NES.prependString "" (nes @"fore")
    , expected: nes @"fore"
    }

  log "contains"
  assert $ NES.contains (Pattern "") (nes @"abcd")
  assert $ NES.contains (Pattern "bc") (nes @"abcd")
  assert $ not NES.contains (Pattern "cb") (nes @"abcd")
  assert $ NES.contains (Pattern "needle") (nes @"haystack with needle")
  assert $ not NES.contains (Pattern "needle") (nes @"haystack")

  log "localeCompare"
  assertEqual
    { actual: NES.localeCompare (nes @"a") (nes @"a")
    , expected: EQ
    }
  assertEqual
    { actual: NES.localeCompare (nes @"a") (nes @"b")
    , expected: LT
    }
  assertEqual
    { actual: NES.localeCompare (nes @"b") (nes @"a")
    , expected: GT
    }

  log "replace"
  assertEqual
    { actual: NES.replace (Pattern "b") (NES.NonEmptyReplacement (nes @"!")) (nes @"abc")
    , expected: nes @"a!c"
    }
  assertEqual
    { actual: NES.replace (Pattern "b") (NES.NonEmptyReplacement (nes @"!")) (nes @"abbc")
    , expected: nes @"a!bc"
    }
  assertEqual
    { actual: NES.replace (Pattern "d") (NES.NonEmptyReplacement (nes @"!")) (nes @"abc")
    , expected: nes @"abc"
    }

  log "replaceAll"
  assertEqual
    { actual: NES.replaceAll (Pattern "[b]") (NES.NonEmptyReplacement (nes @"!")) (nes @"a[b]c")
    , expected: nes @"a!c"
    }
  assertEqual
    { actual: NES.replaceAll (Pattern "[b]") (NES.NonEmptyReplacement (nes @"!")) (nes @"a[b]c[b]")
    , expected: nes @"a!c!"
    }
  assertEqual
    { actual: NES.replaceAll (Pattern "x") (NES.NonEmptyReplacement (nes @"!")) (nes @"abc")
    , expected: nes @"abc"
    }

  log "stripPrefix"
  assertEqual
    { actual: NES.stripPrefix (Pattern "") (nes @"abc")
    , expected: Just (nes @"abc")
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "a") (nes @"abc")
    , expected: Just (nes @"bc")
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "abc") (nes @"abc")
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "!") (nes @"abc")
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "http:") (nes @"http://purescript.org")
    , expected: Just (nes @"//purescript.org")
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "http:") (nes @"https://purescript.org")
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "Hello!") (nes @"Hello!")
    , expected: Nothing
    }

  log "stripSuffix"
  assertEqual
    { actual: NES.stripSuffix (Pattern ".exe") (nes @"purs.exe")
    , expected: Just (nes @"purs")
    }
  assertEqual
    { actual: NES.stripSuffix (Pattern ".exe") (nes @"purs")
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripSuffix (Pattern "Hello!") (nes @"Hello!")
    , expected: Nothing
    }

  log "toLower"
  assertEqual
    { actual: NES.toLower (nes @"bAtMaN")
    , expected: nes @"batman"
    }

  log "toUpper"
  assertEqual
    { actual: NES.toUpper (nes @"bAtMaN")
    , expected: nes @"BATMAN"
    }

  log "trim"
  assertEqual
    { actual: NES.trim (nes @"  abc  ")
    , expected: Just (nes @"abc")
    }
  assertEqual
    { actual: NES.trim (nes @"   \n")
    , expected: Nothing
    }

  log "joinWith"
  assertEqual
    { actual: NES.joinWith "" []
    , expected: ""
    }
  assertEqual
    { actual: NES.joinWith "" [nes @"a", nes @"b"]
    , expected: "ab"
    }
  assertEqual
    { actual: NES.joinWith "--" [nes @"a", nes @"b", nes @"c"]
    , expected: "a--b--c"
    }

  log "join1With"
  assertEqual
    { actual: NES.join1With "" (nea [nes @"a", nes @"b"])
    , expected: nes @"ab"
    }
  assertEqual
    { actual: NES.join1With "--" (nea [nes @"a", nes @"b", nes @"c"])
    , expected: nes @"a--b--c"
    }
  assertEqual
    { actual: NES.join1With ", " (nea [nes @"apple", nes @"banana"])
    , expected: nes @"apple, banana"
    }
  assertEqual
    { actual: NES.join1With "" (nea [nes @"apple", nes @"banana"])
    , expected: nes @"applebanana"
    }

  log "joinWith1"
  assertEqual
    { actual: NES.joinWith1 (nes @" ") (nea ["a", "b"])
    , expected: nes @"a b"
    }
  assertEqual
    { actual: NES.joinWith1 (nes @"--") (nea ["a", "b", "c"])
    , expected: nes @"a--b--c"
    }
  assertEqual
    { actual: NES.joinWith1 (nes @", ") (nea ["apple", "banana"])
    , expected: nes @"apple, banana"
    }
  assertEqual
    { actual: NES.joinWith1 (nes @"/") (nea ["a", "b", "", "c", ""])
    , expected: nes @"a/b//c/"
    }

nea :: Array ~> NEA.NonEmptyArray
nea = unsafePartial fromJust <<< NEA.fromArray

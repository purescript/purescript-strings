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
import Type.Proxy (Proxy(..))

testNonEmptyString :: Effect Unit
testNonEmptyString = do

  log "fromString"
  assertEqual
    { actual: NES.fromString ""
    , expected: Nothing
    }
  assertEqual
    { actual: NES.fromString "hello"
    , expected: Just (nes (Proxy :: Proxy "hello"))
    }

  log "toString"
  assertEqual
    { actual: (NES.toString <$> NES.fromString "hello")
    , expected: Just "hello"
    }

  log "appendString"
  assertEqual
    { actual: NES.appendString (nes (Proxy :: Proxy "Hello")) " world"
    , expected: nes (Proxy :: Proxy "Hello world")
    }
  assertEqual
    { actual: NES.appendString (nes (Proxy :: Proxy "Hello")) ""
    , expected: nes (Proxy :: Proxy "Hello")
    }

  log "prependString"
  assertEqual
    { actual: NES.prependString "be" (nes (Proxy :: Proxy "fore"))
    , expected: nes (Proxy :: Proxy "before")
    }
  assertEqual
    { actual: NES.prependString "" (nes (Proxy :: Proxy "fore"))
    , expected: nes (Proxy :: Proxy "fore")
    }

  log "contains"
  assert $ NES.contains (Pattern "") (nes (Proxy :: Proxy "abcd"))
  assert $ NES.contains (Pattern "bc") (nes (Proxy :: Proxy "abcd"))
  assert $ not NES.contains (Pattern "cb") (nes (Proxy :: Proxy "abcd"))
  assert $ NES.contains (Pattern "needle") (nes (Proxy :: Proxy "haystack with needle"))
  assert $ not NES.contains (Pattern "needle") (nes (Proxy :: Proxy "haystack"))

  log "localeCompare"
  assertEqual
    { actual: NES.localeCompare (nes (Proxy :: Proxy "a")) (nes (Proxy :: Proxy "a"))
    , expected: EQ
    }
  assertEqual
    { actual: NES.localeCompare (nes (Proxy :: Proxy "a")) (nes (Proxy :: Proxy "b"))
    , expected: LT
    }
  assertEqual
    { actual: NES.localeCompare (nes (Proxy :: Proxy "b")) (nes (Proxy :: Proxy "a"))
    , expected: GT
    }

  log "replace"
  assertEqual
    { actual: NES.replace (Pattern "b") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "abc"))
    , expected: nes (Proxy :: Proxy "a!c")
    }
  assertEqual
    { actual: NES.replace (Pattern "b") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "abbc"))
    , expected: nes (Proxy :: Proxy "a!bc")
    }
  assertEqual
    { actual: NES.replace (Pattern "d") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "abc"))
    , expected: nes (Proxy :: Proxy "abc")
    }

  log "replaceAll"
  assertEqual
    { actual: NES.replaceAll (Pattern "[b]") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "a[b]c"))
    , expected: nes (Proxy :: Proxy "a!c")
    }
  assertEqual
    { actual: NES.replaceAll (Pattern "[b]") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "a[b]c[b]"))
    , expected: nes (Proxy :: Proxy "a!c!")
    }
  assertEqual
    { actual: NES.replaceAll (Pattern "x") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "abc"))
    , expected: nes (Proxy :: Proxy "abc")
    }

  log "stripPrefix"
  assertEqual
    { actual: NES.stripPrefix (Pattern "") (nes (Proxy :: Proxy "abc"))
    , expected: Just (nes (Proxy :: Proxy "abc"))
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "a") (nes (Proxy :: Proxy "abc"))
    , expected: Just (nes (Proxy :: Proxy "bc"))
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "abc") (nes (Proxy :: Proxy "abc"))
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "!") (nes (Proxy :: Proxy "abc"))
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "http:") (nes (Proxy :: Proxy "http://purescript.org"))
    , expected: Just (nes (Proxy :: Proxy "//purescript.org"))
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "http:") (nes (Proxy :: Proxy "https://purescript.org"))
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "Hello!") (nes (Proxy :: Proxy "Hello!"))
    , expected: Nothing
    }

  log "stripSuffix"
  assertEqual
    { actual: NES.stripSuffix (Pattern ".exe") (nes (Proxy :: Proxy "purs.exe"))
    , expected: Just (nes (Proxy :: Proxy "purs"))
    }
  assertEqual
    { actual: NES.stripSuffix (Pattern ".exe") (nes (Proxy :: Proxy "purs"))
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripSuffix (Pattern "Hello!") (nes (Proxy :: Proxy "Hello!"))
    , expected: Nothing
    }

  log "toLower"
  assertEqual
    { actual: NES.toLower (nes (Proxy :: Proxy "bAtMaN"))
    , expected: nes (Proxy :: Proxy "batman")
    }

  log "toUpper"
  assertEqual
    { actual: NES.toUpper (nes (Proxy :: Proxy "bAtMaN"))
    , expected: nes (Proxy :: Proxy "BATMAN")
    }

  log "trim"
  assertEqual
    { actual: NES.trim (nes (Proxy :: Proxy "  abc  "))
    , expected: Just (nes (Proxy :: Proxy "abc"))
    }
  assertEqual
    { actual: NES.trim (nes (Proxy :: Proxy "   \n"))
    , expected: Nothing
    }

  log "joinWith"
  assertEqual
    { actual: NES.joinWith "" []
    , expected: ""
    }
  assertEqual
    { actual: NES.joinWith "" [nes (Proxy :: Proxy "a"), nes (Proxy :: Proxy "b")]
    , expected: "ab"
    }
  assertEqual
    { actual: NES.joinWith "--" [nes (Proxy :: Proxy "a"), nes (Proxy :: Proxy "b"), nes (Proxy :: Proxy "c")]
    , expected: "a--b--c"
    }

  log "join1With"
  assertEqual
    { actual: NES.join1With "" (nea [nes (Proxy :: Proxy "a"), nes (Proxy :: Proxy "b")])
    , expected: nes (Proxy :: Proxy "ab")
    }
  assertEqual
    { actual: NES.join1With "--" (nea [nes (Proxy :: Proxy "a"), nes (Proxy :: Proxy "b"), nes (Proxy :: Proxy "c")])
    , expected: nes (Proxy :: Proxy "a--b--c")
    }
  assertEqual
    { actual: NES.join1With ", " (nea [nes (Proxy :: Proxy "apple"), nes (Proxy :: Proxy "banana")])
    , expected: nes (Proxy :: Proxy "apple, banana")
    }
  assertEqual
    { actual: NES.join1With "" (nea [nes (Proxy :: Proxy "apple"), nes (Proxy :: Proxy "banana")])
    , expected: nes (Proxy :: Proxy "applebanana")
    }

  log "joinWith1"
  assertEqual
    { actual: NES.joinWith1 (nes (Proxy :: Proxy " ")) (nea ["a", "b"])
    , expected: nes (Proxy :: Proxy "a b")
    }
  assertEqual
    { actual: NES.joinWith1 (nes (Proxy :: Proxy "--")) (nea ["a", "b", "c"])
    , expected: nes (Proxy :: Proxy "a--b--c")
    }
  assertEqual
    { actual: NES.joinWith1 (nes (Proxy :: Proxy ", ")) (nea ["apple", "banana"])
    , expected: nes (Proxy :: Proxy "apple, banana")
    }
  assertEqual
    { actual: NES.joinWith1 (nes (Proxy :: Proxy "/")) (nea ["a", "b", "", "c", ""])
    , expected: nes (Proxy :: Proxy "a/b//c/")
    }

nea :: Array ~> NEA.NonEmptyArray
nea = unsafePartial fromJust <<< NEA.fromArray

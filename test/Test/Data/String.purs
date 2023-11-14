module Test.Data.String (testString) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert, assertEqual)

testString :: Effect Unit
testString = do

  log "null"
  assert $ S.null ""
  assert $ not (S.null "a")

  log "stripPrefix"
  -- this is a re-export from Data.String.CodeUnits, so the majority of tests are in there
  assertEqual
    { actual: S.stripPrefix (Pattern "𝕒𝕓𝕔") "𝕒𝕓𝕔𝕕𝕖"
    , expected: Just "𝕕𝕖"
    }

  log "stripSuffix"
  -- this is a re-export from Data.String.CodeUnits, so the majority of tests are in there
  assertEqual
    { actual: S.stripSuffix (Pattern "𝕔𝕕𝕖") "𝕒𝕓𝕔𝕕𝕖"
    , expected: Just "𝕒𝕓"
    }

  log "contains"
  assert $ S.contains (Pattern "") ""
  assert $ S.contains (Pattern "") "abcd"
  assert $ S.contains (Pattern "bc") "abcd"
  assert $ not S.contains (Pattern "cb") "abcd"

  log "localeCompare"
  assertEqual
    { actual: S.localeCompare "" ""
    , expected: EQ
    }
  assertEqual
    { actual: S.localeCompare "a" "a"
    , expected: EQ
    }
  assertEqual
    { actual: S.localeCompare "a" "b"
    , expected: LT
    }
  assertEqual
    { actual: S.localeCompare "b" "a"
    , expected: GT
    }

  log "replace"
  assertEqual
    { actual: S.replace (Pattern "b") (Replacement "") "abc"
    , expected: "ac"
    }
  assertEqual
    { actual: S.replace (Pattern "b") (Replacement "!") "abc"
    , expected: "a!c"
    }
  assertEqual
    { actual: S.replace (Pattern "d") (Replacement "!") "abc"
    , expected: "abc"
    }

  log "replaceAll"
  assertEqual
    { actual: S.replaceAll (Pattern "b") (Replacement "") "abbbbbc"
    , expected: "ac"
    }
  assertEqual
    { actual: S.replaceAll (Pattern "[b]") (Replacement "!") "a[b]c"
    , expected: "a!c"
    }

  log "split"
  assertEqual
    { actual: S.split (Pattern "") ""
    , expected: []
    }
  assertEqual
    { actual: S.split (Pattern "") "a"
    , expected: ["a"]
    }
  assertEqual
    { actual: S.split (Pattern "") "ab"
    , expected: ["a", "b"]
    }
  assertEqual
    { actual: S.split (Pattern "b") "aabcc"
    , expected: ["aa", "cc"]
    }
  assertEqual
    { actual: S.split (Pattern "d") "abc"
    , expected: ["abc"]
    }
  assertEqual
    { actual: S.split (Pattern "--") "a--b--c"
    , expected: ["a", "b", "c"]
    }

  log "toLower"
  assertEqual
    { actual: S.toLower "bAtMaN"
    , expected: "batman"
    }

  log "toUpper"
  assertEqual
    { actual: S.toUpper "bAtMaN"
    , expected: "BATMAN"
    }

  log "trim"
  assertEqual
    { actual: S.trim "  abc  "
    , expected: "abc"
    }

  log "joinWith"
  assertEqual
    { actual: S.joinWith "" []
    , expected: ""
    }
  assertEqual
    { actual: S.joinWith "" ["a", "b"]
    , expected: "ab"
    }
  assertEqual
    { actual: S.joinWith "--" ["a", "b", "c"]
    , expected: "a--b--c"
    }

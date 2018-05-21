module Test.Data.Char (testChar) where

import Prelude

import Data.Char as C
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual)

testChar :: Effect Unit
testChar = do

  log "toLower"
  assertEqual
    { actual: C.toLower 'A'
    , expected: 'a'
    }
  assertEqual
    { actual: C.toLower 'a'
    , expected: 'a'
    }

  log "toUpper"
  assertEqual
    { actual: C.toUpper 'a'
    , expected: 'A'
    }
  assertEqual
    { actual: C.toUpper 'A'
    , expected: 'A'
    }

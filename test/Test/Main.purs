module Test.Main where

import Debug
import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf)
import Data.String.CodeUnits as CU
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual, assertEqual')
import Test.Data.String (testString)
import Test.Data.String.CaseInsensitive (testCaseInsensitiveString)
import Test.Data.String.CodePoints (testStringCodePoints)
import Test.Data.String.CodeUnits (testStringCodeUnits)
import Test.Data.String.NonEmpty (testNonEmptyString)
import Test.Data.String.NonEmpty.CodeUnits (testNonEmptyStringCodeUnits)
import Test.Data.String.Regex (testStringRegex)
import Test.Data.String.Unsafe (testStringUnsafe)

foo = "360° sehen können"


-- go = do
--   log $ "LOG: " <> foo
--   traceM foo
--   let _ = spy foo foo
--       bar' = spy "CU.indexOf" $ CU.indexOf (spy "pattern" (Pattern " ")) (spy "string" "360° sehen können")

--       bar = spy "CU.take 5" $ CU.take 5 "360° sehen können"
--       bar'' = spy "CU.take 10" $ CU.take 10 "360° sehen können"


--   pure unit

-- splitOnSpace :: String -> { current :: String, next :: String }
-- splitOnSpace text =
--   case CP.indexOf (Pattern " ") text of
--     Just i ->
--       let
--         { before: current, after: next } = Logger.spy "Splitresult" $ splitAt i text
--       in
--         { current: trim current, next: trim next }
--     Nothing -> { current: text, next: "" }



main :: Effect Unit
main = do
  assertEqual' "CU split utf8" { expected: Just 5, actual: CU.indexOf (Pattern " ") "360° sehen können" }
  assertEqual' "CP normal string" { expected: Just 4, actual: indexOf (Pattern " ") "this is a string" }
  assertEqual' "CP split utf8" { expected: Just 4, actual: indexOf (Pattern " ") "360° sehen können" }

  log "\n--- Data.String ---\n"
  testString
  log "\n--- Data.String.CodePoints ---\n"
  -- testStringCodePoints
  log "\n--- Data.String.CodeUnits ---\n"
  testStringCodeUnits
  log "\n--- Data.String.Unsafe ---\n"
  testStringUnsafe
  log "\n--- Data.String.Regex ---\n"
  testStringRegex
  log "\n--- Data.String.CaseInsensitive ---\n"
  testCaseInsensitiveString
  log "\n--- Data.String.NonEmpty ---\n"
  testNonEmptyString
  log "\n--- Data.String.NonEmpty.CodeUnits ---\n"
  testNonEmptyStringCodeUnits

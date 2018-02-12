module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT)
import Test.Data.Char (testChar)
import Test.Data.String (testString)
import Test.Data.String.CaseInsensitive (testCaseInsensitiveString)
import Test.Data.String.CodePoints (testStringCodePoints)
import Test.Data.String.NonEmpty (testNonEmptyString)
import Test.Data.String.Regex (testStringRegex)
import Test.Data.String.Unsafe (testStringUnsafe)

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "\n--- Data.Char ---\n"
  testChar
  log "\n--- Data.String ---\n"
  testString
  log "\n--- Data.String.CodePoints ---\n"
  testStringCodePoints
  log "\n--- Data.String.Unsafe ---\n"
  testStringUnsafe
  log "\n--- Data.String.Regex ---\n"
  testStringRegex
  log "\n--- Data.String.CaseInsensitive ---\n"
  testCaseInsensitiveString
  log "\n--- Data.String.NonEmpty ---\n"
  testNonEmptyString

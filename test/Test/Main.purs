module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Assert (ASSERT)
import Test.Data.Char (testChar)
import Test.Data.String (testString)
import Test.Data.String.CodePoints (testStringCodePoints)
import Test.Data.String.Regex (testStringRegex)
import Test.Data.String.Unsafe (testStringUnsafe)
import Test.Data.String.CaseInsensitive (testCaseInsensitiveString)

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  testChar
  testString
  testStringCodePoints
  testStringUnsafe
  testStringRegex
  testCaseInsensitiveString

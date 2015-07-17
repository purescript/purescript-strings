module Test.Main where

import Prelude
import Test.Data.Char
import Test.Data.String
import Test.Data.String.Unsafe
import Test.Data.String.Regex

main = do
  testChar
  testString
  testStringUnsafe
  testStringRegex

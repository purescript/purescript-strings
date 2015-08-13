-- | A type and functions for single characters.
module Data.Char
  ( toString
  , fromCharCode
  , toCharCode
  , toLower
  , toUpper
  ) where

import Prelude

-- | Returns the string of length `1` containing only the given character.
foreign import toString :: Char -> String

-- | Returns the numeric Unicode value of the character.
foreign import toCharCode :: Char -> Int

-- | Constructs a character from the given Unicode numeric value.
foreign import fromCharCode :: Int -> Char

-- | Converts a character to lowercase.
foreign import toLower :: Char -> Char

-- | Converts a character to uppercase.
foreign import toUpper :: Char -> Char

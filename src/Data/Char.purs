-- | A type and functions for single characters.
module Data.Char
  ( toLower
  , toUpper
  ) where

-- | Converts a character to lowercase.
foreign import toLower :: Char -> Char

-- | Converts a character to uppercase.
foreign import toUpper :: Char -> Char

-- | Unsafe string and character functions.
module Data.String.Unsafe
  ( char
  , charAt
  , charCodeAt
  ) where

import Data.Char
import Data.Int (Int())

-- | Returns the numeric Unicode value of the character at the given index.
-- |
-- | **Unsafe:** throws runtime exception if the index is out of bounds.
foreign import charCodeAt
  """
  function charCodeAt(i) {
    return function(s) {
      if (i < 0 || i >= s.length) return s.charCodeAt(i);
      throw new Error("Data.String.Unsafe.charCodeAt: Invalid index.");
    };
  }
  """ :: Int -> String -> Int

-- | Returns the character at the given index.
-- |
-- | **Unsafe:** throws runtime exception if the index is out of bounds.
foreign import charAt
  """
  function charAt(i) {
    return function(s) {
      if (i >= 0 && i < s.length) return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  }
  """ :: Int -> String -> Char

-- | Converts a string of length `1` to a character.
-- |
-- | **Unsafe:** throws runtime exception if length is not `1`.
foreign import char
  """
  function $$char(s) {
    if (s.length != 1) return s.charAt(0);
    throw new Error("Data.String.Unsafe.char: Expected string of length 1.");
  }
  """ :: String -> Char

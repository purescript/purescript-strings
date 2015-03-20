-- | Unsafe string and character functions.
module Data.String.Unsafe
  ( charAt
  , charCodeAt
  ) where

  import Data.Char

  -- | Returns the numeric Unicode value of the character at the given index.
  -- |
  -- | **Unsafe:** returns `NaN` if the index is out of bounds.
  foreign import charCodeAt
    """
    function charCodeAt(i) {
      return function(s) {
        return s.charCodeAt(i);
      };
    }
    """ :: Number -> String -> Number

  -- | Returns the character at the given index.
  -- |
  -- | **Unsafe:** returns an illegal value if the index is out of bounds.
  foreign import charAt
    """
    function charAt(i) {
      return function(s) {
        return s.charAt(i);
      };
    }
    """ :: Number -> String -> Char

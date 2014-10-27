module Data.String.Unsafe
  ( charAt
  , charCodeAt
  ) where

  import Data.Char

  foreign import charCodeAt
    """
    function charCodeAt(i) {
      return function(s) {
        return s.charCodeAt(i);
      };
    }
    """ :: Number -> String -> Number

  foreign import charAt
    """
    function charAt(i) {
      return function(s) {
        return s.charAt(i);
      };
    }
    """ :: Number -> String -> Char

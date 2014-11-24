module Data.String
  (
    charAt,
    charCodeAt,
    fromCharArray,
    fromChar,
    indexOf,
    indexOf',
    lastIndexOf,
    lastIndexOf',
    null,
    uncons,
    length,
    singleton,
    localeCompare,
    replace,
    count,
    take,
    takeWhile,
    drop,
    dropWhile,
    split,
    toCharArray,
    toLower,
    toUpper,
    trim,
    joinWith
  ) where

  import Data.Maybe
  import Data.Char
  import Data.Function
  import qualified Data.String.Unsafe as U

  foreign import _charAt
    """
    function _charAt(i, s, Just, Nothing) {
      return i >= 0 && i < s.length ? Just(s.charAt(i)) : Nothing;
    }
    """ :: forall a. Fn4 Number String (a -> Maybe a) (Maybe a) (Maybe Char)

  charAt :: Number -> String -> Maybe Char
  charAt n s = runFn4 _charAt n s Just Nothing

  fromChar :: Char -> String
  fromChar = charString

  singleton :: Char -> String
  singleton = fromChar

  foreign import _charCodeAt
    """
    function _charCodeAt(i, s, Just, Nothing) {
      return i >= 0 && i < s.length ? Just(s.charCodeAt(i)) : Nothing;
    }
    """ :: forall a. Fn4 Number String (a -> Maybe a) (Maybe a) (Maybe Number)

  charCodeAt :: Number -> String -> Maybe Number
  charCodeAt n s = runFn4 _charCodeAt n s Just Nothing

  null :: String -> Boolean
  null s = length s == 0

  uncons :: String -> Maybe {head :: Char, tail :: String}
  uncons s | null s = Nothing
  uncons s = Just {head : U.charAt 0 s, tail : drop 1 s}

  takeWhile :: (Char -> Boolean) -> String -> String
  takeWhile p s = take (count p s) s

  dropWhile :: (Char -> Boolean) -> String -> String
  dropWhile p s = drop (count p s) s

  foreign import fromCharArray
    """
    function fromCharArray(a) {
      return a.join('');
    }
    """ :: [Char] -> String

  foreign import indexOf
    """
    function indexOf(x) {
      return function(s) {
        return s.indexOf(x);
      };
    }
    """ :: String -> String -> Number

  foreign import indexOf'
    """
    function indexOf$prime(x) {
      return function(startAt) {
        return function(s) {
          return s.indexOf(x, startAt);
        };
      };
    }
    """ :: String -> Number -> String -> Number

  foreign import lastIndexOf
    """
    function lastIndexOf(x) {
      return function(s) {
        return s.lastIndexOf(x);
      };
    }
    """ :: String -> String -> Number

  foreign import lastIndexOf'
    """
    function lastIndexOf$prime(x) {
      return function(startAt) {
        return function(s) {
          return s.lastIndexOf(x, startAt);
        };
      };
    }
    """ :: String -> Number -> String -> Number

  foreign import length
    """
    function length(s) {
      return s.length;
    }
    """ :: String -> Number

  foreign import localeCompare
    """
    function localeCompare(s1) {
      return function(s2) {
        return s1.localeCompare(s2);
      };
    }
    """ :: String -> String -> Number

  foreign import replace
    """
    function replace(s1) {
      return function(s2) {
        return function(s3) {
          return s3.replace(s1, s2);
        };
      };
    }
    """ :: String -> String -> String -> String

  foreign import take
    """
    function take(n) {
      return function(s) {
        return s.substr(0, n);
      };
    }
    """ :: Number -> String -> String

  foreign import drop
    """
    function drop(n) {
      return function(s) {
        return s.substr(n);
      };
    }
    """ :: Number -> String -> String

  foreign import count
    """
    function count(p){      
      return function(s){
        var i;
        for(i = 0; i < s.length && p(s.charAt(i)); i++){};
        return i;
      };      
    }
    """ :: (Char -> Boolean) -> String -> Number

  foreign import split
    """
    function split(sep) {
      return function(s) {
        return s.split(sep);
      };
    }
    """ :: String -> String -> [String]

  foreign import toCharArray
    """
    function toCharArray(s) {
      return s.split('');
    }
    """ :: String -> [Char]

  foreign import toLower
    """
    function toLower(s) {
      return s.toLowerCase();
    }
    """ :: String -> String

  foreign import toUpper
    """
    function toUpper(s) {
      return s.toUpperCase();
    }
    """ :: String -> String

  foreign import trim
    """
    function trim(s) {
      return s.trim();
    }
    """ :: String -> String

  foreign import joinWith
    """
    function joinWith(s) {
      return function(xs) {
        return xs.join(s);
      };
    }
    """ :: String -> [String] -> String

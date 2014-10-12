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
    length,
    localeCompare,
    replace,
    take,
    drop,
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

  foreign import _charAt
    "function _charAt(i, s, Just, Nothing) {\
    \  if (i < 0 || i >= s.length) return Nothing;\
    \  else return Just(s.charAt(i));\
    \}" :: forall a. Fn4 Number String (a -> Maybe a) (Maybe a) (Maybe Char)

  charAt :: Number -> String -> Maybe Char
  charAt n s = runFn4 _charAt n s Just Nothing

  fromChar :: Char -> String
  fromChar = charString

  foreign import charCodeAt
    "function charCodeAt(i) {\
    \  return function(s) {\
    \    return s.charCodeAt(i); \
    \  };\
    \}" :: Number -> String -> Number

  foreign import fromCharArray
    "function fromCharArray(a) {\
    \   return a.join('');  \
    \" :: [Char] -> String

  foreign import indexOf
    "function indexOf(x) {\
    \  return function(s) {\
    \    return s.indexOf(x);\
    \  }; \
    \}" :: String -> String -> Number

  foreign import indexOf'
    "function indexOf$prime(x) {\
    \  return function(startAt) {\
    \    return function(s) {\
    \      return s.indexOf(x, startAt);\
    \    }; \
    \  }; \
    \}" :: String -> Number -> String -> Number

  foreign import lastIndexOf
    "function lastIndexOf(x) {\
    \  return function(s) {\
    \    return s.lastIndexOf(x);\
    \  };\
    \}" :: String -> String -> Number

  foreign import lastIndexOf'
    "function lastIndexOf$prime(x) {\
    \  return function(startAt) {\
    \    return function(s) {\
    \      return s.lastIndexOf(x, startAt);\
    \    }; \
    \  }; \
    \}" :: String -> Number -> String -> Number

  foreign import length
    "function length(s) {\
    \  return s.length;\
    \}" :: String -> Number

  foreign import localeCompare
    "function localeCompare(s1) {\
    \  return function(s2) {\
    \    return s1.localeCompare(s2);\
    \  };\
    \}" :: String -> String -> Number

  foreign import replace
    "function replace(s1) {\
    \  return function(s2) {\
    \    return function(s3) {\
    \      return s3.replace(s1, s2);\
    \    };\
    \  };\
    \}" :: String -> String -> String -> String

  foreign import take
    "function take(n) {\
    \  return function(s) {\
    \    return s.substr(0, n);\
    \  };\
    \}" :: Number -> String -> String

  foreign import drop
    "function drop(n) {\
    \  return function(s) {\
    \    return s.substr(n);\
    \  };\
    \}" :: Number -> String -> String

  foreign import split
    "function split(sep) {\
    \  return function(s) {\
    \    return s.split(sep);\
    \  };\
    \}" :: String -> String -> [String]

  foreign import toCharArray 
    "function toCharArray(s) {\
    \   return s.split('');\
    \}" :: String -> [Char]

  foreign import toLower
    "function toLower(s) {\
    \  return s.toLowerCase();\
    \}" :: String -> String

  foreign import toUpper
    "function toUpper(s) {\
    \  return s.toUpperCase();\
    \}" :: String -> String

  foreign import trim
    "function trim(s) {\
    \  return s.trim();\
    \}" :: String -> String

  foreign import joinWith
    "function joinWith (s) {\
    \  return function (xs) {\
    \    return xs.join(s);\
    \  };\
    \}" :: String -> [String] -> String

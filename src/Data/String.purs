module Data.String
  (
    Char(),
    charAt,
    charAt',
    charCodeAt,
    charCodeOf,
    fromArray,
    fromCharCode,
    fromCharCode',
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
    toArray,
    toLower,
    toUpper,
    trim,
    joinWith
  ) where

  import Data.Maybe

  newtype Char = Char String 

  -- | Deprecated
  foreign import charAt
    "function charAt(i) {\
    \  return function(s) {\
    \    return s.charAt(i); \
    \  };\
    \}" :: Number -> String -> String

  charAt' :: Number -> String -> Maybe Char
  charAt' n s = 
    case charAt n s of 
      "" -> Nothing
      c  -> Just $ Char c

  charCodeOf :: Char -> Number
  charCodeOf (Char s) = charCodeAt 0 s

  foreign import charCodeAt
    "function charCodeAt(i) {\
    \  return function(s) {\
    \    return s.charCodeAt(i); \
    \  };\
    \}" :: Number -> String -> Number

  foreign import fromArray
    "function fromArray(a) {\
    \   return a.join('');  \
    \" :: [Char] -> String

  -- | Deprecated
  foreign import fromCharCode
    "function fromCharCode(n) {\
    \  return String.fromCharCode(n);\
    \}" :: Number -> String

  fromCharCode' :: Number -> Char
  fromCharCode' c = Char $ fromCharCode c

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

  foreign import toArray 
    "function toArray(s) {\
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

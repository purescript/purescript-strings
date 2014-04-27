module Data.String where

foreign import charAt
  "function charAt(i) {\
  \  return function(s) {\
  \    return s.charAt(i); \
  \  };\
  \}" :: Number -> String -> String

foreign import charCodeAt
  "function charCodeAt(i) {\
  \  return function(s) {\
  \    return s.charCodeAt(i); \
  \  };\
  \}" :: Number -> String -> Number

foreign import fromCharCode
  "function fromCharCode(n) {\
  \  return String.fromCharCode(n);\
  \}" :: Number -> String

foreign import indexOf
  "function indexOf(x) {\
  \  return function(s) {\
  \    return s.indexOf(x);\
  \  }; \
  \}" :: String -> String -> Number

foreign import lastIndexOf
  "function lastIndexOf(x) {\
  \  return function(s) {\
  \    return s.lastIndexOf(x);\
  \  };\
  \}" :: String -> String -> Number

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

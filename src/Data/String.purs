module Data.String where

foreign import charAt
  "function charAt(i) {\
  \  return function(s) {\
  \    return s.charAt(i); \
  \  };\
  \}" :: Number -> String -> String

foreign import fromCharCode
  "function fromCharCode(n) {\
  \  return String.fromCharCode(n);\
  \}" :: Number -> String

foreign import indexOf
  "function indexOf(s1) {\
  \  return function(s2) {\
  \    return s1.indexOf(s2);\
  \  }; \
  \}" :: String -> String -> Number

foreign import lastIndexOf
  "function lastIndexOf(s1) {\
  \  return function(s2) {\
  \    return s1.lastIndexOf(s2);\
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

foreign import slice
  "function slice(st) {\
  \  return function(e) {\
  \    return function(s) {\
  \      return s.slice(st, e);\
  \    };\
  \  };\
  \}" :: Number -> Number -> String -> String

foreign import split
  "function split(sep) {\
  \  return function(s) {\
  \    return s.split(sep);\
  \  };\
  \}" :: String -> String -> [String]

foreign import substr
  "function substr(n1) {\
  \  return function(n2) {\
  \    return function(s) {\
  \      return s.substr(n1, n2);\
  \    };\
  \  };\
  \}" :: Number -> Number -> String -> String

foreign import substring
  "function substring(n1) {\
  \  return function(n2) {\
  \    return function(s) {\
  \      return s.substring(n1, n2);\
  \    };\
  \  };\
  \}" :: Number -> Number -> String -> String

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
  "function joinWith (l) {\
  \  return function (s) {\
  \    return l.join(s);\
  \  };\
  \}" :: [String] -> String -> String

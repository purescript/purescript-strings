module Data.String where

foreign import lengthS
  "function lengthS(s) {\
  \  return s.length;\
  \}" :: String -> Number

foreign import charAt
  "function charAt(i) {\
  \  return function(s) {\
  \    return s.charAt(i); \
  \  };\
  \}" :: Number -> String -> String

foreign import indexOfS
  "function indexOfS(s1) {\
  \  return function(s2) {\
  \    return s1.indexOf(s2);\
  \  }; \
  \}" :: String -> String -> Number

foreign import lastIndexOfS
  "function lastIndexOfS(s1) {\
  \  return function(s2) {\
  \    return s1.lastIndexOf(s2);\
  \  };\
  \}" :: String -> String -> Number

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

foreign import sliceS
  "function sliceS(st) {\
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

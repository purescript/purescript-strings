module Data.String.Regex where

foreign import data Regex :: *

foreign import regex
  "function regex(s1) {\
  \  return function(s2) {\
  \    return new RegExp(s1, s2);\
  \  };\
  \}" :: String -> String -> Regex

foreign import test
  "function test(r) {\
  \  return function (s) {\
  \    return r.test(s);\
  \  };\
  \}" :: Regex -> String -> Boolean

foreign import match
  "function match(r) {\
  \  return function (s) {\
  \    return s.match(r); \
  \  };\
  \}" :: Regex -> String -> [String]

foreign import replace
  "function replace(r) {\
  \  return function(s1) {\
  \    return function(s2) {\
  \      return s2.replace(r, s1);\
  \    };\
  \  };\
  \}" :: Regex -> String -> String -> String

foreign import replace'
  "function replace$prime(r) {\
  \  return function(f) {\
  \    return function(s2) {\
  \      return s2.replace(r, function (match) {\
  \        return f(match)(Array.prototype.splice.call(arguments, 1, arguments.length - 3));\
  \      });\
  \    };\
  \  };\
  \}" :: Regex -> (String -> [String] -> String) -> String -> String

foreign import search
  "function search(r) {\
  \  return function (s) {\
  \    return s.search(r);\
  \  };\
  \}" :: Regex -> String -> Number

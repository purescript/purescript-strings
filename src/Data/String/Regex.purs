module Data.String.Regex where

foreign import data Regex :: *

foreign import regex
  "function regex(s1) {\
  \  return function(s2) {\
  \    return new Regex(s1, s2);\
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

foreign import replaceR
  "function replaceR(r) {\
  \  return function(s1) {\
  \    return function(s2) {\
  \      return s2.replace(r, s1);\
  \    };\
  \  };\
  \}" :: Regex -> String -> String -> String

foreign import search
  "function search(r) {\
  \  return function (s) {\
  \    return s.search(r);\
  \  };\
  \}" :: Regex -> String -> Number

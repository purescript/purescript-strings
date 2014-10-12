module Data.Char
  ( Char(),
    charString,
    fromCharCode,
    toCharCode
  ) where

  newtype Char = Char String

  charString :: Char -> String
  charString (Char s) = s

  foreign import toCharCode
    "function toCharCode(c){\
    \  return c.charCodeAt(0);\
    \}" :: Char -> Number

  foreign import fromCharCode 
    "function fromCharCode(c){\
    \   return String.fromCharCode(c);\
    \}" :: Number -> Char


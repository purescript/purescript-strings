module Data.Char
  ( Char(),
    charString
  ) where

  newtype Char = Char String

  charString :: Char -> String
  charString (Char s) = s
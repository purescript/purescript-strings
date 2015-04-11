-- | A type and functions for single characters.
module Data.Char
  ( Char()
  , toString
  , fromCharCode
  , toCharCode
  ) where

import Data.Int (Int(), fromNumber)

--| A unicode character.
newtype Char = Char String

-- | Returns the string of length `1` containing only the given character.
toString :: Char -> String
toString (Char s) = s

-- | Returns the numeric Unicode value of the character.
foreign import toCharCode
  """
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  """ :: Char -> Int

-- | Constructs a character from the given Unicode numeric value.
foreign import fromCharCode
  """
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }
  """ :: Int -> Char

-- | Characters can be compared for equality with `==` and `/=`.
instance eqChar :: Eq Char where
  (==) (Char a) (Char b) = a == b
  (/=) a b = not (a == b)

-- | Characters can be compared with `compare`, `>`, `>=`, `<` and `<=`.
instance ordChar :: Ord Char where
  compare (Char a) (Char b) = a `compare` b

-- | Characters fall within the Unicode range.
instance boundedChar :: Bounded Char where
  top = fromCharCode zero
  bottom = fromCharCode (fromNumber 65535)

-- | Characters can be rendered as a string with `show`.
instance showChar :: Show Char where
  show (Char s) = "Char " ++ show s

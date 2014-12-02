module Data.Char
  ( Char(),
    charString,
    fromCharCode,
    toCharCode,
    isSpace,
    isDigit,
    isOctDigit,
    isHexDigit,
    charInRange
  ) where

  newtype Char = Char String

  charString :: Char -> String
  charString (Char s) = s

  foreign import toCharCode
    """
    function toCharCode(c) {
      return c.charCodeAt(0);
    }
    """ :: Char -> Number

  foreign import fromCharCode
    """
    function fromCharCode(c) {
      return String.fromCharCode(c);
    }
    """ :: Number -> Char

  instance eqChar :: Eq Char where
    (==) (Char a) (Char b) = a == b

    (/=) a b = not (a == b)

  instance ordChar :: Ord Char where
    compare (Char a) (Char b) = a `compare` b

  instance showChar :: Show Char where
    show (Char s) = "Char " ++ show s
  
  
  isSpace :: Char -> Boolean
  isSpace (Char c) =
       c == " "
    || c == "\t"
    || c == "\n"
    || c == "\r"
    || c == "\f"
    || c == "\v"
    || c == "\xa0"
  
  charInRange :: Char -> Char -> Char -> Boolean
  charInRange st en c =
       toCharCode c >= toCharCode st
    && toCharCode c <= toCharCode en
  
  isDigit :: Char -> Boolean
  isDigit = charInRange (Char "0") (Char "9")

  isOctDigit :: Char -> Boolean
  isOctDigit = charInRange (Char "0") (Char "9")
  
  isHexDigit :: Char -> Boolean
  isHexDigit c =
       isDigit c
    || charInRange (Char "a") (Char "f") c
    || charInRange (Char "A") (Char "F") c

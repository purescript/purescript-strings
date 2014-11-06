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

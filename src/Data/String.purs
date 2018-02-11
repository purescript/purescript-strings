-- | Wraps the functions of Javascript's `String` object.
-- | A String represents a sequence of characters.
-- | For details of the underlying implementation, see [String Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String).
module Data.String
  ( Pattern(..)
  , Replacement(..)
  , charAt
  , charCodeAt
  , fromCharArray
  , toChar
  , contains
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , null
  , uncons
  , length
  , singleton
  , localeCompare
  , replace
  , replaceAll
  , take
  , takeRight
  , takeWhile
  , drop
  , dropRight
  , dropWhile
  , stripPrefix
  , stripSuffix
  , count
  , split
  , splitAt
  , toCharArray
  , toLower
  , toUpper
  , trim
  , joinWith
  ) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.String.Unsafe as U

-- | A newtype used in cases where there is a string to be matched.
-- |
-- | ```purescript
-- | pursPattern = Pattern ".purs"
-- | --can be used like this:
-- | contains pursPattern "Test.purs"
-- |    == true
-- | ```
-- |
newtype Pattern = Pattern String

derive instance eqPattern :: Eq Pattern
derive instance ordPattern :: Ord Pattern
derive instance newtypePattern :: Newtype Pattern _

instance showPattern :: Show Pattern where
  show (Pattern s) = "(Pattern " <> show s <> ")"

-- | A newtype used in cases to specify a replacement for a pattern.
newtype Replacement = Replacement String

derive instance eqReplacement :: Eq Replacement
derive instance ordReplacement :: Ord Replacement
derive instance newtypeReplacement :: Newtype Replacement _

instance showReplacement :: Show Replacement where
  show (Replacement s) = "(Replacement " <> show s <> ")"

-- | Returns the character at the given index, if the index is within bounds.
-- |
-- | ```purescript
-- | charAt 2 "Hello" == Just 'l'
-- | charAt 10 "Hello" == Nothing
-- | ```
-- |
charAt :: Int -> String -> Maybe Char
charAt = _charAt Just Nothing

foreign import _charAt
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Int
  -> String
  -> Maybe Char

-- | Returns a string of length `1` containing the given character.
-- |
-- | ```purescript
-- | singleton 'l' == "l"
-- | ```
-- |
foreign import singleton :: Char -> String

-- | Returns the numeric Unicode value of the character at the given index,
-- | if the index is within bounds.
-- | ```purescript
-- | charCodeAt 2 "5 €" == Just 0x20AC
-- | charCodeAt 10 "5 €" == Nothing
-- | ```
-- |
charCodeAt :: Int -> String -> Maybe Int
charCodeAt = _charCodeAt Just Nothing

foreign import _charCodeAt
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Int
  -> String
  -> Maybe Int

-- | Converts the string to a character, if the length of the string is
-- | exactly `1`.
-- |
-- | ```purescript
-- | toChar "l" == Just 'l'
-- | toChar "Hi" == Nothing -- since length is not 1
-- | ```
-- |
toChar :: String -> Maybe Char
toChar = _toChar Just Nothing

foreign import _toChar
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> String
  -> Maybe Char

-- | Returns `true` if the given string is empty.
-- |
-- | ```purescript
-- | null "" == true
-- | null "Hi" == false
-- | ```
-- |
null :: String -> Boolean
null s = s == ""

-- | Returns the first character and the rest of the string,
-- | if the string is not empty.
-- |
-- | ```purescript
-- | uncons "" == Nothing
-- | uncons "Hello World" == Just { head: 'H', tail: "ello World" }
-- | ```
-- |
uncons :: String -> Maybe { head :: Char, tail :: String }
uncons "" = Nothing
uncons s  = Just { head: U.charAt zero s, tail: drop one s }

-- | Returns the longest prefix (possibly empty) of characters that satisfy
-- | the predicate.
-- |
-- | ```purescript
-- | takeWhile (_ /= ':') "http://purescript.org" == "http"
-- | ```
-- |
takeWhile :: (Char -> Boolean) -> String -> String
takeWhile p s = take (count p s) s

-- | Returns the suffix remaining after `takeWhile`.
-- |
-- | ```purescript
-- | dropWhile (_ /= '.') "Test.purs" == ".purs"
-- | ```
-- |
dropWhile :: (Char -> Boolean) -> String -> String
dropWhile p s = drop (count p s) s

-- | If the string starts with the given prefix, return the portion of the
-- | string left after removing it, as a Just value. Otherwise, return Nothing.
-- |
-- | ```purescript
-- | stripPrefix (Pattern "http:") "http://purescript.org" == Just "//purescript.org"
-- | stripPrefix (Pattern "http:") "https://purescript.org" == Nothing
-- | ```
-- |
stripPrefix :: Pattern -> String -> Maybe String
stripPrefix prefix@(Pattern prefixS) str =
  case indexOf prefix str of
    Just 0 -> Just $ drop (length prefixS) str
    _ -> Nothing

-- | If the string ends with the given suffix, return the portion of the
-- | string left after removing it, as a `Just` value. Otherwise, return
-- | `Nothing`.
-- |
-- | ```purescript
-- | stripSuffix (Pattern ".exe") "psc.exe" == Just "psc"
-- | stripSuffix (Pattern ".exe") "psc" == Nothing
-- | ```
-- |
stripSuffix :: Pattern -> String -> Maybe String
stripSuffix suffix@(Pattern suffixS) str =
  case lastIndexOf suffix str of
    Just x | x == length str - length suffixS -> Just $ take x str
    _ -> Nothing

-- | Converts an array of characters into a string.
-- |
-- | ```purescript
-- | fromCharArray ['H', 'e', 'l', 'l', 'o'] == "Hello"
-- | ```
-- |
foreign import fromCharArray :: Array Char -> String

-- | Checks whether the pattern appears in the given string.
-- |
-- | ```purescript
-- | contains (Pattern "needle") "haystack with needle" == true
-- | contains (Pattern "needle") "haystack" == false
-- | ```
-- |
contains :: Pattern -> String -> Boolean
contains pat = isJust <<< indexOf pat

-- | Returns the index of the first occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | indexOf (Pattern "c") "abcdc" == Just 2
-- | indexOf (Pattern "c") "aaa" == Nothing
-- | ```
-- |
indexOf :: Pattern -> String -> Maybe Int
indexOf = _indexOf Just Nothing

foreign import _indexOf
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> String
  -> Maybe Int

-- | Returns the index of the first occurrence of the pattern in the
-- | given string, starting at the specified index. Returns `Nothing` if there is
-- | no match.
-- |
-- | ```purescript
-- | indexOf' (Pattern "a") 2 "ababa" == Just 2
-- | indexOf' (Pattern "a") 3 "ababa" == Just 4
-- | ```
-- |
indexOf' :: Pattern -> Int -> String -> Maybe Int
indexOf' = _indexOf' Just Nothing

foreign import _indexOf'
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> Int
  -> String
  -> Maybe Int

-- | Returns the index of the last occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | lastIndexOf (Pattern "c") "abcdc" == Just 4
-- | lastIndexOf (Pattern "c") "aaa" == Nothing
-- | ```
-- |
lastIndexOf :: Pattern -> String -> Maybe Int
lastIndexOf = _lastIndexOf Just Nothing

foreign import _lastIndexOf
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> String
  -> Maybe Int

-- | Returns the index of the last occurrence of the pattern in the
-- | given string, starting at the specified index
-- | and searching backwards towards the beginning of the string.
-- | Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | lastIndexOf' (Pattern "a") 1 "ababa" == Just 0
-- | lastIndexOf' (Pattern "a") 3 "ababa" == Just 2
-- | lastIndexOf' (Pattern "a") 4 "ababa" == Just 4
-- | ```
-- |
lastIndexOf' :: Pattern -> Int -> String -> Maybe Int
lastIndexOf' = _lastIndexOf' Just Nothing

foreign import _lastIndexOf'
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> Int
  -> String
  -> Maybe Int

-- | Returns the number of characters the string is composed of.
-- |
-- | ```purescript
-- | length "Hello World" == 11
-- | ```
-- |
foreign import length :: String -> Int

-- | Compare two strings in a locale-aware fashion. This is in contrast to
-- | the `Ord` instance on `String` which treats strings as arrays of code
-- | units:
-- |
-- | ```purescript
-- | "ä" `localeCompare` "b" == LT
-- | "ä" `compare` "b" == GT
-- | ```
-- |
localeCompare :: String -> String -> Ordering
localeCompare = _localeCompare LT EQ GT

foreign import _localeCompare
  :: Ordering
  -> Ordering
  -> Ordering
  -> String
  -> String
  -> Ordering

-- | Replaces the first occurence of the pattern with the replacement string.
-- |
-- | ```purescript
-- | replace (Pattern "<=") (Replacement "≤") "a <= b <= c" == "a ≤ b <= c"
-- | ```
-- |
foreign import replace :: Pattern -> Replacement -> String -> String

-- | Replaces all occurences of the pattern with the replacement string.
-- |
-- | ```purescript
-- | replaceAll (Pattern "<=") (Replacement "≤") "a <= b <= c" == "a ≤ b ≤ c"
-- | ```
-- |
foreign import replaceAll :: Pattern -> Replacement -> String -> String

-- | Returns the first `n` characters of the string.
-- |
-- | ```purescript
-- | take 5 "Hello World" == "Hello"
-- | ```
-- |
foreign import take :: Int -> String -> String

-- | Returns the last `n` characters of the string.
-- |
-- | ```purescript
-- | take 5 "Hello World" == "World"
-- | ```
-- |
takeRight :: Int -> String -> String
takeRight i s = drop (length s - i) s

-- | Returns the string without the first `n` characters.
-- |
-- | ```purescript
-- | drop 6 "Hello World" == "World"
-- | ```
-- |
foreign import drop :: Int -> String -> String

-- | Returns the string without the last `n` characters.
-- |
-- | ```purescript
-- | dropRight 6 "Hello World" == "Hello"
-- | ```
-- |
dropRight :: Int -> String -> String
dropRight i s = take (length s - i) s

-- | Returns the number of contiguous characters at the beginning
-- | of the string for which the predicate holds.
-- |
-- | ```purescript
-- | count (_ /= ' ') "Hello World" == 5 -- since length "Hello" == 5
-- | ```
-- |
foreign import count :: (Char -> Boolean) -> String -> Int

-- | Returns the substrings of the second string separated along occurences
-- | of the first string.
-- |
-- | ```purescript
-- | split (Pattern " ") "hello world" == ["hello", "world"]
-- | ```
-- |
foreign import split :: Pattern -> String -> Array String

-- | Returns the substrings of a split at the given index, if the index is within bounds.
-- |
-- | ```purescript
-- | splitAt 2 "Hello World" == Just { before: "He", after: "llo World"}
-- | splitAt 10 "Hi" == Nothing
-- | ```
-- |
splitAt :: Int -> String -> Maybe { before :: String, after :: String }
splitAt = _splitAt Just Nothing

foreign import _splitAt :: (forall a. a -> Maybe a)
                        -> (forall a. Maybe a)
                        -> Int
                        -> String
                        -> Maybe { before :: String, after :: String }

-- | Converts the string into an array of characters.
-- |
-- | ```purescript
-- | toCharArray "Hello☺\n" == ['H','e','l','l','o','☺','\n']
-- | ```
-- |
foreign import toCharArray :: String -> Array Char

-- | Returns the argument converted to lowercase.
-- |
-- | ```purescript
-- | toLower "hElLo" == "hello"
-- | ```
-- |
foreign import toLower :: String -> String

-- | Returns the argument converted to uppercase.
-- |
-- | ```purescript
-- | toUpper "Hello" == "HELLO"
-- | ```
-- |
foreign import toUpper :: String -> String

-- | Removes whitespace from the beginning and end of a string, including
-- | [whitespace characters](http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
-- | and [line terminators](http://www.ecma-international.org/ecma-262/5.1/#sec-7.3).
-- |
-- | ```purescript
-- | trim "   Hello  \n World\n\t    " == "Hello  \n World"
-- | ```
-- |
foreign import trim :: String -> String

-- | Joins the strings in the array together, inserting the first argument
-- | as separator between them.
-- |
-- | ```purescript
-- | joinWith ", " ["apple", "banana", "orange"] == "apple, banana, orange"
-- | ```
-- |
foreign import joinWith :: String -> Array String -> String

-- | Wraps the functions of Javascript's `String` object.
-- | A String represents a sequence of characters.
-- | For details of the underlying implementation, see [String Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String).
module Data.String
  ( module Data.String.Pattern
  , contains
  , null
  , localeCompare
  , replace
  , replaceAll
  , stripPrefix
  , stripSuffix
  , split
  , toLower
  , toUpper
  , trim
  , joinWith
  ) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..), Replacement(..))

-- | Returns `true` if the given string is empty.
-- |
-- | ```purescript
-- | null "" == true
-- | null "Hi" == false
-- | ```
null :: String -> Boolean
null s = s == ""

-- | If the string starts with the given prefix, return the portion of the
-- | string left after removing it, as a Just value. Otherwise, return Nothing.
-- |
-- | ```purescript
-- | stripPrefix (Pattern "http:") "http://purescript.org" == Just "//purescript.org"
-- | stripPrefix (Pattern "http:") "https://purescript.org" == Nothing
-- | ```
stripPrefix :: Pattern -> String -> Maybe String
stripPrefix prefix@(Pattern prefixS) str =
  case SCU.indexOf prefix str of
    Just 0 -> Just $ SCU.drop (SCU.length prefixS) str
    _ -> Nothing

-- | If the string ends with the given suffix, return the portion of the
-- | string left after removing it, as a `Just` value. Otherwise, return
-- | `Nothing`.
-- |
-- | ```purescript
-- | stripSuffix (Pattern ".exe") "psc.exe" == Just "psc"
-- | stripSuffix (Pattern ".exe") "psc" == Nothing
-- | ```
stripSuffix :: Pattern -> String -> Maybe String
stripSuffix suffix@(Pattern suffixS) str =
  case SCU.lastIndexOf suffix str of
    Just x | x == SCU.length str - SCU.length suffixS -> Just $ SCU.take x str
    _ -> Nothing

-- | Checks whether the pattern appears in the given string.
-- |
-- | ```purescript
-- | contains (Pattern "needle") "haystack with needle" == true
-- | contains (Pattern "needle") "haystack" == false
-- | ```
contains :: Pattern -> String -> Boolean
contains pat = isJust <<< SCU.indexOf pat

-- | Compare two strings in a locale-aware fashion. This is in contrast to
-- | the `Ord` instance on `String` which treats strings as arrays of code
-- | units:
-- |
-- | ```purescript
-- | "ä" `localeCompare` "b" == LT
-- | "ä" `compare` "b" == GT
-- | ```
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
foreign import replace :: Pattern -> Replacement -> String -> String

-- | Replaces all occurences of the pattern with the replacement string.
-- |
-- | ```purescript
-- | replaceAll (Pattern "<=") (Replacement "≤") "a <= b <= c" == "a ≤ b ≤ c"
-- | ```
foreign import replaceAll :: Pattern -> Replacement -> String -> String

-- | Returns the substrings of the second string separated along occurences
-- | of the first string.
-- |
-- | ```purescript
-- | split (Pattern " ") "hello world" == ["hello", "world"]
-- | ```
foreign import split :: Pattern -> String -> Array String

-- | Returns the argument converted to lowercase.
-- |
-- | ```purescript
-- | toLower "hElLo" == "hello"
-- | ```
foreign import toLower :: String -> String

-- | Returns the argument converted to uppercase.
-- |
-- | ```purescript
-- | toUpper "Hello" == "HELLO"
-- | ```
foreign import toUpper :: String -> String

-- | Removes whitespace from the beginning and end of a string, including
-- | [whitespace characters](http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
-- | and [line terminators](http://www.ecma-international.org/ecma-262/5.1/#sec-7.3).
-- |
-- | ```purescript
-- | trim "   Hello  \n World\n\t    " == "Hello  \n World"
-- | ```
foreign import trim :: String -> String

-- | Joins the strings in the array together, inserting the first argument
-- | as separator between them.
-- |
-- | ```purescript
-- | joinWith ", " ["apple", "banana", "orange"] == "apple, banana, orange"
-- | ```
foreign import joinWith :: String -> Array String -> String

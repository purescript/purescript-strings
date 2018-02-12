-- | Non-empty strings.
-- |
-- | Please note that the examples in this documentation use a notation like
-- | `NonEmptyString "..."` for demonstration purposes, `NonEmptyString` cannot
-- | be created directly like that, as we can't prove the string is non-empty to
-- | the compiler at compile-time.
module Data.String.NonEmpty
  ( NonEmptyString
  , NonEmptyReplacement(..)
  , fromString
  , unsafeFromString
  , fromCharArray
  , singleton
  , cons
  , snoc
  , fromFoldable1
  , toString
  , toCharArray
  , charAt
  , charCodeAt
  , toChar
  , appendString
  , prependString
  , contains
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , uncons
  , length
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
  , splitAt
  , toLower
  , toUpper
  , trim
  , joinWith
  , join1With
  , joinWith1
  , module Data.String
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Foldable as F
import Data.Maybe (Maybe(..), fromJust)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Foldable as F1
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Unsafe as U
import Unsafe.Coerce (unsafeCoerce)

-- | A string that is known not to be empty.
newtype NonEmptyString = NonEmptyString String

derive newtype instance eqNonEmptyString ∷ Eq NonEmptyString
derive newtype instance ordNonEmptyString ∷ Ord NonEmptyString
derive newtype instance semigroupNonEmptyString ∷ Semigroup NonEmptyString

instance showNonEmptyString :: Show NonEmptyString where
  show (NonEmptyString s) = "(NonEmptyString.unsafeFromString " <> show s <> ")"

-- | A newtype used in cases to specify a non-empty replacement for a pattern.
newtype NonEmptyReplacement = NonEmptyReplacement NonEmptyString

derive newtype instance eqNonEmptyReplacement :: Eq NonEmptyReplacement
derive newtype instance ordNonEmptyReplacement :: Ord NonEmptyReplacement
derive newtype instance semigroupNonEmptyReplacement ∷ Semigroup NonEmptyReplacement

instance showNonEmptyReplacement :: Show NonEmptyReplacement where
  show (NonEmptyReplacement s) = "(NonEmptyReplacement " <> show s <> ")"

-- | Creates a `NonEmptyString` from a `String`, returning `Nothing` if the
-- | input is empty.
-- |
-- | ```purescript
-- | fromString "" = Nothing
-- | fromString "hello" = Just (NonEmptyString "hello")
-- | ```
fromString :: String -> Maybe NonEmptyString
fromString = case _ of
  "" -> Nothing
  s -> Just (NonEmptyString s)

-- | A partial version of `fromString`.
unsafeFromString :: Partial => String -> NonEmptyString
unsafeFromString = fromJust <<< fromString

-- | Creates a `NonEmptyString` from a character array `String`, returning
-- | `Nothing` if the input is empty.
-- |
-- | ```purescript
-- | fromCharArray [] = Nothing
-- | fromCharArray ['a', 'b', 'c'] = Just (NonEmptyString "abc")
-- | ```
fromCharArray :: Array Char -> Maybe NonEmptyString
fromCharArray = case _ of
  [] -> Nothing
  cs -> Just (NonEmptyString (String.fromCharArray cs))

-- | Creates a `NonEmptyString` from a character.
singleton :: Char -> NonEmptyString
singleton = NonEmptyString <<< String.singleton

-- | Creates a `NonEmptyString` from a string by prepending a character.
-- |
-- | ```purescript
-- | cons 'a' "bc" = NonEmptyString "abc"
-- | cons 'a' "" = NonEmptyString "a"
-- | ```
cons :: Char -> String -> NonEmptyString
cons c s = NonEmptyString (String.singleton c <> s)

-- | Creates a `NonEmptyString` from a string by appending a character.
-- |
-- | ```purescript
-- | snoc 'c' "ab" = NonEmptyString "abc"
-- | snoc 'a' "" = NonEmptyString "a"
-- | ```
snoc :: Char -> String -> NonEmptyString
snoc c s = NonEmptyString (s <> String.singleton c)

-- | Creates a `NonEmptyString` from a `Foldable1` container carrying
-- | characters.
fromFoldable1 :: forall f. Foldable1 f => f Char -> NonEmptyString
fromFoldable1 = F1.fold1 <<< coe
  where
    coe ∷ f Char -> f NonEmptyString
    coe = unsafeCoerce

-- | Converts a `NonEmptyString` back into a standard `String`.
toString :: NonEmptyString -> String
toString (NonEmptyString s) = s

-- | Returns the character at the given index, if the index is within bounds.
-- |
-- | ```purescript
-- | charAt 2 (NonEmptyString "Hello") == Just 'l'
-- | charAt 10 (NonEmptyString "Hello") == Nothing
-- | ```
charAt :: Int -> NonEmptyString -> Maybe Char
charAt = liftS <<< String.charAt

-- | Returns the numeric Unicode value of the character at the given index,
-- | if the index is within bounds.
-- |
-- | ```purescript
-- | charCodeAt 2 (NonEmptyString "5 €") == Just 0x20AC
-- | charCodeAt 10 (NonEmptyString "5 €") == Nothing
-- | ```
charCodeAt :: Int -> NonEmptyString -> Maybe Int
charCodeAt = liftS <<< String.charCodeAt

-- | Converts the `NonEmptyString` to a character, if the length of the string
-- | is exactly `1`.
-- |
-- | ```purescript
-- | toChar "H" == Just 'H'
-- | toChar "Hi" == Nothing
-- | ```
toChar :: NonEmptyString -> Maybe Char
toChar (NonEmptyString s) = String.toChar s

-- | Converts the `NonEmptyString` into an array of characters.
-- |
-- | ```purescript
-- | toCharArray (NonEmptyString "Hello☺\n") == ['H','e','l','l','o','☺','\n']
-- | ```
toCharArray :: NonEmptyString -> Array Char
toCharArray (NonEmptyString s) = String.toCharArray s

-- | Appends a string to this non-empty string. Since one of the strings is
-- | non-empty we know the result will be too.
-- |
-- | ```purescript
-- | appendString (NonEmptyString "Hello") " world" == NonEmptyString "Hello world"
-- | appendString (NonEmptyString "Hello") "" == NonEmptyString "Hello"
-- | ```
appendString :: NonEmptyString -> String -> NonEmptyString
appendString (NonEmptyString s1) s2 = NonEmptyString (s1 <> s2)

-- | Prepends a string to this non-empty string. Since one of the strings is
-- | non-empty we know the result will be too.
-- |
-- | ```purescript
-- | prependString "be" (NonEmptyString "fore") == NonEmptyString "before"
-- | prependString "" (NonEmptyString "fore") == NonEmptyString "fore"
-- | ```
prependString :: String -> NonEmptyString -> NonEmptyString
prependString s1 (NonEmptyString s2) = NonEmptyString (s1 <> s2)

-- | Returns the first character and the rest of the string.
-- |
-- | ```purescript
-- | uncons "a" == { head: 'a', tail: Nothing }
-- | uncons "Hello World" == { head: 'H', tail: Just (NonEmptyString "ello World") }
-- | ```
uncons :: NonEmptyString -> { head :: Char, tail :: Maybe NonEmptyString }
uncons (NonEmptyString s) =
  { head: U.charAt 0 s
  , tail: fromString (String.drop 1 s)
  }

-- | Returns the longest prefix of characters that satisfy the predicate.
-- | `Nothing` is returned if there is no matching prefix.
-- |
-- | ```purescript
-- | takeWhile (_ /= ':') (NonEmptyString "http://purescript.org") == Just (NonEmptyString "http")
-- | takeWhile (_ == 'a') (NonEmptyString "xyz") == Nothing
-- | ```
takeWhile :: (Char -> Boolean) -> NonEmptyString -> Maybe NonEmptyString
takeWhile f = fromString <<< liftS (String.takeWhile f)

-- | Returns the suffix remaining after `takeWhile`.
-- |
-- | ```purescript
-- | dropWhile (_ /= '.') (NonEmptyString "Test.purs") == Just (NonEmptyString ".purs")
-- | ```
dropWhile :: (Char -> Boolean) -> NonEmptyString -> Maybe NonEmptyString
dropWhile f = fromString <<< liftS (String.dropWhile f)

-- | If the string starts with the given prefix, return the portion of the
-- | string left after removing it. If the prefix does not match or there is no
-- | remainder, the result will be `Nothing`.
-- |
-- | ```purescript
-- | stripPrefix (Pattern "http:") (NonEmptyString "http://purescript.org") == Just (NonEmptyString "//purescript.org")
-- | stripPrefix (Pattern "http:") (NonEmptyString "https://purescript.org") == Nothing
-- | stripPrefix (Pattern "Hello!") (NonEmptyString "Hello!") == Nothing
-- | ```
stripPrefix :: Pattern -> NonEmptyString -> Maybe NonEmptyString
stripPrefix pat = fromString <=< liftS (String.stripPrefix pat)

-- | If the string ends with the given suffix, return the portion of the
-- | string left after removing it. If the suffix does not match or there is no
-- | remainder, the result will be `Nothing`.
-- |
-- | ```purescript
-- | stripSuffix (Pattern ".exe") (NonEmptyString "purs.exe") == Just (NonEmptyString "purs")
-- | stripSuffix (Pattern ".exe") (NonEmptyString "purs") == Nothing
-- | stripSuffix (Pattern "Hello!") (NonEmptyString "Hello!") == Nothing
-- | ```
stripSuffix :: Pattern -> NonEmptyString -> Maybe NonEmptyString
stripSuffix pat = fromString <=< liftS (String.stripSuffix pat)

-- | Checks whether the pattern appears in the given string.
-- |
-- | ```purescript
-- | contains (Pattern "needle") (NonEmptyString "haystack with needle") == true
-- | contains (Pattern "needle") (NonEmptyString "haystack") == false
-- | ```
contains :: Pattern -> NonEmptyString -> Boolean
contains = liftS <<< String.contains

-- | Returns the index of the first occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | indexOf (Pattern "c") (NonEmptyString "abcdc") == Just 2
-- | indexOf (Pattern "c") (NonEmptyString "aaa") == Nothing
-- | ```
indexOf :: Pattern -> NonEmptyString -> Maybe Int
indexOf = liftS <<< String.indexOf

-- | Returns the index of the first occurrence of the pattern in the
-- | given string, starting at the specified index. Returns `Nothing` if there is
-- | no match.
-- |
-- | ```purescript
-- | indexOf' (Pattern "a") 2 (NonEmptyString "ababa") == Just 2
-- | indexOf' (Pattern "a") 3 (NonEmptyString "ababa") == Just 4
-- | ```
indexOf' :: Pattern -> Int -> NonEmptyString -> Maybe Int
indexOf' pat = liftS <<< String.indexOf' pat

-- | Returns the index of the last occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | lastIndexOf (Pattern "c") (NonEmptyString "abcdc") == Just 4
-- | lastIndexOf (Pattern "c") (NonEmptyString "aaa") == Nothing
-- | ```
lastIndexOf :: Pattern -> NonEmptyString -> Maybe Int
lastIndexOf = liftS <<< String.lastIndexOf

-- | Returns the index of the last occurrence of the pattern in the
-- | given string, starting at the specified index
-- | and searching backwards towards the beginning of the string.
-- | Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | lastIndexOf' (Pattern "a") 1 (NonEmptyString "ababa") == Just 0
-- | lastIndexOf' (Pattern "a") 3 (NonEmptyString "ababa") == Just 2
-- | lastIndexOf' (Pattern "a") 4 (NonEmptyString "ababa") == Just 4
-- | ```
lastIndexOf' :: Pattern -> Int -> NonEmptyString -> Maybe Int
lastIndexOf' pat = liftS <<< String.lastIndexOf' pat

-- | Returns the number of characters the string is composed of.
-- |
-- | ```purescript
-- | length (NonEmptyString "Hello World") == 11
-- | ```
length :: NonEmptyString -> Int
length (NonEmptyString s) = String.length s

-- | Compare two strings in a locale-aware fashion. This is in contrast to
-- | the `Ord` instance on `String` which treats strings as arrays of code
-- | units:
-- |
-- | ```purescript
-- | NonEmptyString "ä" `localeCompare` NonEmptyString "b" == LT
-- | NonEmptyString "ä" `compare` NonEmptyString "b" == GT
-- | ```
localeCompare :: NonEmptyString -> NonEmptyString -> Ordering
localeCompare (NonEmptyString a) (NonEmptyString b) = String.localeCompare a b

-- | Replaces the first occurence of the pattern with the replacement string.
-- |
-- | ```purescript
-- | replace (Pattern "<=") (NonEmptyReplacement "≤") (NonEmptyString "a <= b <= c") == NonEmptyString "a ≤ b <= c"
-- | ```
replace :: Pattern -> NonEmptyReplacement -> NonEmptyString -> NonEmptyString
replace pat (NonEmptyReplacement (NonEmptyString rep)) (NonEmptyString s) =
  NonEmptyString (String.replace pat (String.Replacement rep) s)

-- | Replaces all occurences of the pattern with the replacement string.
-- |
-- | ```purescript
-- | replaceAll (Pattern "<=") (NonEmptyReplacement "≤") (NonEmptyString "a <= b <= c") == NonEmptyString "a ≤ b ≤ c"
-- | ```
replaceAll :: Pattern -> NonEmptyReplacement -> NonEmptyString -> NonEmptyString
replaceAll pat (NonEmptyReplacement (NonEmptyString rep)) (NonEmptyString s) =
  NonEmptyString (String.replaceAll pat (String.Replacement rep) s)

-- | Returns the first `n` characters of the string. Returns `Nothing` if `n` is
-- | less than 1.
-- |
-- | ```purescript
-- | take 5 (NonEmptyString "Hello World") == Just (NonEmptyString "Hello")
-- | take 0 (NonEmptyString "Hello World") == Nothing
-- | ```
take :: Int -> NonEmptyString -> Maybe NonEmptyString
take i (NonEmptyString s)
  | i < 1 = Nothing
  | otherwise = Just (NonEmptyString (String.take i s))

-- | Returns the last `n` characters of the string. Returns `Nothing` if `n` is
-- | less than 1.
-- |
-- | ```purescript
-- | take 5 (NonEmptyString "Hello World") == Just (NonEmptyString "World")
-- | take 0 (NonEmptyString "Hello World") == Nothing
-- | ```
takeRight :: Int -> NonEmptyString -> Maybe NonEmptyString
takeRight i (NonEmptyString s)
  | i < 1 = Nothing
  | otherwise = Just (NonEmptyString (String.takeRight i s))

-- | Returns the string without the first `n` characters. Returns `Nothing` if
-- | more characters are dropped than the string is long.
-- |
-- | ```purescript
-- | drop 6 (NonEmptyString "Hello World") == Just (NonEmptyString "World")
-- | drop 20 (NonEmptyString "Hello World") == Nothing
-- | ```
drop :: Int -> NonEmptyString -> Maybe NonEmptyString
drop i (NonEmptyString s)
  | i >= String.length s = Nothing
  | otherwise = Just (NonEmptyString (String.drop i s))

-- | Returns the string without the last `n` characters. Returns `Nothing` if
-- | more characters are dropped than the string is long.
-- |
-- | ```purescript
-- | dropRight 6 (NonEmptyString "Hello World") == Just (NonEmptyString "Hello")
-- | dropRight 20 (NonEmptyString "Hello World") == Nothing
-- | ```
dropRight :: Int -> NonEmptyString -> Maybe NonEmptyString
dropRight i (NonEmptyString s)
  | i >= String.length s = Nothing
  | otherwise = Just (NonEmptyString (String.dropRight i s))

-- | Returns the number of contiguous characters at the beginning of the string
-- | for which the predicate holds.
-- |
-- | ```purescript
-- | count (_ /= 'o') (NonEmptyString "Hello World") == 4
-- | ```
count :: (Char -> Boolean) -> NonEmptyString -> Int
count = liftS <<< String.count

-- | Returns the substrings of a split at the given index, if the index is
-- | within bounds.
-- |
-- | ```purescript
-- | splitAt 2 (NonEmptyString "Hello World") == Just { before: Just (NonEmptyString "He"), after: Just (NonEmptyString "llo World") }
-- | splitAt 10 (NonEmptyString "Hi") == Nothing
-- | ```
splitAt
  :: Int
  -> NonEmptyString
  -> Maybe { before :: Maybe NonEmptyString, after :: Maybe NonEmptyString }
splitAt i (NonEmptyString s) = case String.splitAt i s of
  Just { before, after } ->
    Just { before: fromString before, after: fromString after }
  Nothing ->
    Nothing

-- | Returns the argument converted to lowercase.
-- |
-- | ```purescript
-- | toLower (NonEmptyString "hElLo") == NonEmptyString "hello"
-- | ```
toLower :: NonEmptyString -> NonEmptyString
toLower (NonEmptyString s) = NonEmptyString (String.toLower s)

-- | Returns the argument converted to uppercase.
-- |
-- | ```purescript
-- | toUpper (NonEmptyString "Hello") == NonEmptyString "HELLO"
-- | ```
toUpper :: NonEmptyString -> NonEmptyString
toUpper (NonEmptyString s) = NonEmptyString (String.toUpper s)

-- | Removes whitespace from the beginning and end of a string, including
-- | [whitespace characters](http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
-- | and [line terminators](http://www.ecma-international.org/ecma-262/5.1/#sec-7.3).
-- | If the string is entirely made up of whitespace the result will be Nothing.
-- |
-- | ```purescript
-- | trim (NonEmptyString "   Hello  \n World\n\t    ") == Just (NonEmptyString "Hello  \n World")
-- | trim (NonEmptyString "   \n") == Nothing
-- | ```
trim :: NonEmptyString -> Maybe NonEmptyString
trim (NonEmptyString s) = fromString (String.trim s)

-- | Joins the strings in a container together as a new string, inserting the
-- | first argument as separator between them. The result is not guaranteed to
-- | be non-empty.
-- |
-- | ```purescript
-- | joinWith ", " [NonEmptyString "apple", NonEmptyString "banana"] == "apple, banana"
-- | joinWith ", " [] == ""
-- | ```
joinWith :: forall f. Foldable f => String -> f NonEmptyString -> String
joinWith splice = F.intercalate splice <<< coe
  where
    coe :: f NonEmptyString -> f String
    coe = unsafeCoerce

-- | Joins non-empty strings in a non-empty container together as a new
-- | non-empty string, inserting a possibly empty string as separator between
-- | them. The result is guaranteed to be non-empty.
-- |
-- | ```purescript
-- | -- array syntax is used for demonstration here, it would need to be a real `Foldable1`
-- | join1With ", " [NonEmptyString "apple", NonEmptyString "banana"] == NonEmptyString "apple, banana"
-- | join1With "" [NonEmptyString "apple", NonEmptyString "banana"] == NonEmptyString "applebanana"
-- | ```
join1With :: forall f. Foldable1 f => String -> f NonEmptyString -> NonEmptyString
join1With splice = NonEmptyString <<< joinWith splice

-- | Joins possibly empty strings in a non-empty container together as a new
-- | non-empty string, inserting a non-empty string as a separator between them.
-- | The result is guaranteed to be non-empty.
-- |
-- | ```purescript
-- | -- array syntax is used for demonstration here, it would need to be a real `Foldable1`
-- | joinWith1 (NonEmptyString ", ") ["apple", "banana"] == NonEmptyString "apple, banana"
-- | joinWith1 (NonEmptyString "/") ["a", "b", "", "c", ""] == NonEmptyString "a/b//c/"
-- | ```
joinWith1 :: forall f. Foldable1 f => NonEmptyString -> f String -> NonEmptyString
joinWith1 (NonEmptyString splice) = NonEmptyString <<< F.intercalate splice

liftS :: forall r. (String -> r) -> NonEmptyString -> r
liftS f (NonEmptyString s) = f s

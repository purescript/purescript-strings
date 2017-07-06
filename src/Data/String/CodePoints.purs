-- | These functions allow PureScript strings to be treated as if they were
-- | sequences of Unicode code points instead of their true underlying
-- | implementation (sequences of UTF-16 code units). For nearly all uses of
-- | strings, these functions should be preferred over the ones in Data.String.
module Data.String.CodePoints
  ( module StringReExports
  , CodePoint()
  , codePointAt
  , codePointFromInt
  , codePointToInt
  , count
  , drop
  , dropWhile
  , fromCodePointArray
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , length
  , singleton
  , splitAt
  , take
  , takeWhile
  , toCodePointArray
  , uncons
  ) where

import Prelude

import Data.Array as Array
import Data.Char as Char
import Data.List (List(Cons, Nil), fromFoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as String
import Data.String.Unsafe as Unsafe
-- WARN: In order for this module to be a drop-in replacement for Data.String,
-- this list must be updated to re-export any exports added to Data.String.
import Data.String (Pattern(..), Replacement(..), charAt, charCodeAt, contains, fromCharArray, joinWith, localeCompare, null, replace, replaceAll, split, stripPrefix, stripSuffix, toChar, toCharArray, toLower, toUpper, trim) as StringReExports
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)


-- | CodePoint is an Int bounded between 0 and 0x10FFFF, corresponding to
-- | Unicode code points.
newtype CodePoint = CodePoint Int

derive instance eqCodePoint :: Eq CodePoint
derive instance ordCodePoint :: Ord CodePoint

-- I would prefer that this smart constructor not need to exist and instead
-- CodePoint just implements Enum, but the Enum module already depends on this
-- one. To avoid the circular dependency, we just expose these two functions.
codePointFromInt :: Int -> Maybe CodePoint
codePointFromInt n | 0 <= n && n <= 0x10FFFF = Just (CodePoint n)
codePointFromInt n = Nothing

codePointToInt :: CodePoint -> Int
codePointToInt (CodePoint n) = n

unsurrogate :: Int -> Int -> CodePoint
unsurrogate lead trail = CodePoint ((lead - 0xD800) * 0x400 + (trail - 0xDC00) + 0x10000)

isLead :: Int -> Boolean
isLead cu = 0xD800 <= cu && cu <= 0xDBFF

isTrail :: Int -> Boolean
isTrail cu = 0xDC00 <= cu && cu <= 0xDFFF

fromCharCode :: Int -> String
fromCharCode = String.singleton <<< Char.fromCharCode

-- WARN: this function expects the String parameter to be non-empty
unsafeCodePointAt0 :: String -> CodePoint
unsafeCodePointAt0 = _unsafeCodePointAt0 unsafeCodePointAt0Fallback

foreign import _unsafeCodePointAt0
  :: (String -> CodePoint)
  -> String
  -> CodePoint

unsafeCodePointAt0Fallback :: String -> CodePoint
unsafeCodePointAt0Fallback s =
  if isLead cu0 && isTrail cu1
     then unsurrogate cu0 cu1
     else CodePoint cu0
  where
    cu0 = Unsafe.charCodeAt 0 s
    cu1 = Unsafe.charCodeAt 1 s


-- | Returns the first code point of the string after dropping the given number
-- | of code points from the beginning, if there is such a code point. Operates
-- | in constant space and in time linear to `n`.
codePointAt :: Int -> String -> Maybe CodePoint
codePointAt 0 "" = Nothing
codePointAt 0 s = Just (unsafeCodePointAt0 s)
codePointAt n s = _codePointAt codePointAtFallback Just Nothing unsafeCodePointAt0 n s

foreign import _codePointAt
  :: (Int -> String -> Maybe CodePoint)
  -> (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> (String -> CodePoint)
  -> Int
  -> String
  -> Maybe CodePoint

codePointAtFallback :: Int -> String -> Maybe CodePoint
codePointAtFallback n s = Array.index (toCodePointArray s) n


-- | Returns the number of code points in the leading sequence of code points
-- | which all match the given predicate. Operates in constant space and in
-- | time linear to the length of the given string.
count :: (CodePoint -> Boolean) -> String -> Int
count = _count isLead isTrail unsurrogate

foreign import _count
  :: (Int -> Boolean)
  -> (Int -> Boolean)
  -> (Int -> Int -> CodePoint)
  -> (CodePoint -> Boolean)
  -> String
  -> Int


-- | Drops the given number of code points from the beginning of the given
-- | string. If the string does not have that many code points, returns the
-- | empty string. Operates in space and time linear to the length of the given
-- | string.
drop :: Int -> String -> String
drop n s = fromCodePointArray (Array.drop n (toCodePointArray s))


-- | Drops the leading sequence of code points which all match the given
-- | predicate from the given string. Operates in space and time linear to the
-- | length of the given string.
dropWhile :: (CodePoint -> Boolean) -> String -> String
dropWhile p s = drop (count p s) s


-- | Creates a string from an array of code points. Operates in space and time
-- | linear to the length of the given array.
fromCodePointArray :: Array CodePoint -> String
fromCodePointArray = _fromCodePointArray singletonFallback

foreign import _fromCodePointArray
  :: (CodePoint -> String)
  -> Array CodePoint
  -> String

-- | Returns the number of code points preceding the first match of the given
-- | pattern in the given string. Returns Nothing when no matches are found.
indexOf :: String.Pattern -> String -> Maybe Int
indexOf p s = (\i -> length (String.take i s)) <$> String.indexOf p s


-- | Returns the number of code points preceding the first match of the given
-- | pattern in the given string. Pattern matches preceding the given index
-- | will be ignored. Returns Nothing when no matches are found.
indexOf' :: String.Pattern -> Int -> String -> Maybe Int
indexOf' p i s =
  let s' = drop i s in
  (\k -> i + length (String.take k s')) <$> String.indexOf p s'


-- | Returns the number of code points preceding the last match of the given
-- | pattern in the given string. Returns Nothing when no matches are found.
lastIndexOf :: String.Pattern -> String -> Maybe Int
lastIndexOf p s = (\i -> length (String.take i s)) <$> String.lastIndexOf p s


-- | Returns the number of code points preceding the first match of the given
-- | pattern in the given string. Pattern matches following the given index
-- | will be ignored. Returns Nothing when no matches are found.
lastIndexOf' :: String.Pattern -> Int -> String -> Maybe Int
lastIndexOf' p i s =
  let i' = String.length (take i s) in
  (\k -> length (String.take k s)) <$> String.lastIndexOf' p i' s


-- | Returns the number of code points in the given string. Operates in
-- | constant space and time linear to the length of the string.
length :: String -> Int
length = Array.length <<< toCodePointArray


-- | Creates a string containing just the given code point. Operates in
-- | constant space and time.
singleton :: CodePoint -> String
singleton = _singleton singletonFallback

foreign import _singleton
  :: (CodePoint -> String)
  -> CodePoint
  -> String

singletonFallback :: CodePoint -> String
singletonFallback (CodePoint cp) | cp <= 0xFFFF = fromCharCode cp
singletonFallback (CodePoint cp) = fromCharCode lead <> fromCharCode trail
  where
    lead = ((cp - 0x10000) / 0x400) + 0xD800
    trail = (cp - 0x10000) `mod` 0x400 + 0xDC00


-- | Returns a record with strings created from the code points on either side
-- | of the given index. If the index is not within the string, Nothing is
-- | returned.
splitAt :: Int -> String -> Maybe { before :: String, after :: String }
splitAt i s =
  let cps = toCodePointArray s in
  if i < 0 || Array.length cps < i
    then Nothing
    else Just {
        before: fromCodePointArray (Array.take i cps),
        after: fromCodePointArray (Array.drop i cps)
      }


-- | Returns a string containing the given number of code points from the
-- | beginning of the given string. If the string does not have that many code
-- | points, returns the empty string. Operates in space and time linear to the
-- | given number.
take :: Int -> String -> String
take = _take takeFallback

foreign import _take :: (Int -> String -> String) -> Int -> String -> String

takeFallback :: Int -> String -> String
takeFallback n s = fromCodePointArray (Array.take n (toCodePointArray s))


-- | Returns a string containing the leading sequence of code points which all
-- | match the given predicate from the given string. Operates in space and
-- | time linear to the given number.
takeWhile :: (CodePoint -> Boolean) -> String -> String
takeWhile p s = take (count p s) s


-- | Creates an array of code points from a string. Operates in space and time
-- | linear to the length of the given string.
toCodePointArray :: String -> Array CodePoint
toCodePointArray = _toCodePointArray toCodePointArrayFallback unsafeCodePointAt0

foreign import _toCodePointArray
  :: (String -> Array CodePoint)
  -> (String -> CodePoint)
  -> String
  -> Array CodePoint

toCodePointArrayFallback :: String -> Array CodePoint
toCodePointArrayFallback s = unfoldr decode (fromFoldable (Char.toCharCode <$> String.toCharArray s))
  where
  decode :: List Int -> Maybe (Tuple CodePoint (List Int))
  decode (Cons cu0 (Cons cu1 rest)) | isLead cu0 && isTrail cu1
    = Just (Tuple (unsurrogate cu0 cu1) rest)
  decode (Cons cu rest) = Just (Tuple (CodePoint cu) rest)
  decode Nil = Nothing


-- | Returns a record with the first code point and the remaining code points
-- | of the given string. Returns Nothing if the string is empty. Operates in
-- | space and time linear to the length of the string.
uncons :: String -> Maybe { head :: CodePoint, tail :: String }
uncons s  = { head: _, tail: drop 1 s } <$> codePointAt 0 s

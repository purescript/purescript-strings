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
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as String
import Data.String.Unsafe as Unsafe
-- WARN: If a new function is added to Data.String, a version of that function
-- should be exported from this module, which should be the same except that it
-- should operate on the code point level rather than the code unit level. If
-- the function's behaviour does not change based on whether we consider
-- strings as sequences of code points or code units, it can simply be
-- re-exported from Data.String.
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
  let cu0 = Unsafe.charCodeAt 0 s in
  let cu1 = Unsafe.charCodeAt 1 s in
  if isLead cu0 && isTrail cu1
     then unsurrogate cu0 cu1
     else CodePoint cu0


-- | Returns the first code point of the string after dropping the given number
-- | of code points from the beginning, if there is such a code point. Operates
-- | in constant space and in time linear to the given index.
codePointAt :: Int -> String -> Maybe CodePoint
codePointAt n _ | n < 0 = Nothing
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
codePointAtFallback n s = case uncons s of
  Just { head, tail } -> if n == 0 then Just head else codePointAtFallback (n - 1) tail
  _ -> Nothing


-- | Returns the number of code points in the leading sequence of code points
-- | which all match the given predicate. Operates in constant space and in
-- | time linear to the length of the string.
count :: (CodePoint -> Boolean) -> String -> Int
count = _count countFallback unsafeCodePointAt0

foreign import _count
  :: ((CodePoint -> Boolean) -> String -> Int)
  -> (String -> CodePoint)
  -> (CodePoint -> Boolean)
  -> String
  -> Int

countFallback :: (CodePoint -> Boolean) -> String -> Int
countFallback p s = countTail p s 0

countTail :: (CodePoint -> Boolean) -> String -> Int -> Int
countTail p s accum = case uncons s of
  Just { head, tail } -> if p head then countTail p tail (accum + 1) else accum
  _ -> accum


-- | Drops the given number of code points from the beginning of the string. If
-- | the string does not have that many code points, returns the empty string.
-- | Operates in constant space and in time linear to the given number.
drop :: Int -> String -> String
drop n s = String.drop (String.length (take n s)) s


-- | Drops the leading sequence of code points which all match the given
-- | predicate from the string. Operates in constant space and in time linear
-- | to the length of the string.
dropWhile :: (CodePoint -> Boolean) -> String -> String
dropWhile p s = drop (count p s) s


-- | Creates a string from an array of code points. Operates in space and time
-- | linear to the length of the array.
fromCodePointArray :: Array CodePoint -> String
fromCodePointArray = _fromCodePointArray singletonFallback

foreign import _fromCodePointArray
  :: (CodePoint -> String)
  -> Array CodePoint
  -> String

-- | Returns the number of code points preceding the first match of the given
-- | pattern in the string. Returns Nothing when no matches are found.
indexOf :: String.Pattern -> String -> Maybe Int
indexOf p s = (\i -> length (String.take i s)) <$> String.indexOf p s


-- | Returns the number of code points preceding the first match of the given
-- | pattern in the string. Pattern matches preceding the given index will be
-- | ignored. Returns Nothing when no matches are found.
indexOf' :: String.Pattern -> Int -> String -> Maybe Int
indexOf' p i s =
  let s' = drop i s in
  (\k -> i + length (String.take k s')) <$> String.indexOf p s'


-- | Returns the number of code points preceding the last match of the given
-- | pattern in the string. Returns Nothing when no matches are found.
lastIndexOf :: String.Pattern -> String -> Maybe Int
lastIndexOf p s = (\i -> length (String.take i s)) <$> String.lastIndexOf p s


-- | Returns the number of code points preceding the first match of the given
-- | pattern in the string. Pattern matches following the given index will be
-- | ignored. Returns Nothing when no matches are found.
lastIndexOf' :: String.Pattern -> Int -> String -> Maybe Int
lastIndexOf' p i s =
  let i' = String.length (take i s) in
  (\k -> length (String.take k s)) <$> String.lastIndexOf' p i' s


-- | Returns the number of code points in the string. Operates in constant
-- | space and in time linear to the length of the string.
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
singletonFallback (CodePoint cp) =
  let lead = ((cp - 0x10000) / 0x400) + 0xD800 in
  let trail = (cp - 0x10000) `mod` 0x400 + 0xDC00 in
  fromCharCode lead <> fromCharCode trail


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
-- | points, returns the empty string. Operates in constant space and in time
-- | linear to the given number.
take :: Int -> String -> String
take = _take takeFallback

foreign import _take :: (Int -> String -> String) -> Int -> String -> String

takeFallback :: Int -> String -> String
takeFallback n _ | n < 1 = ""
takeFallback n s = case uncons s of
  Just { head, tail } -> singleton head <> takeFallback (n - 1) tail
  _ -> s


-- | Returns a string containing the leading sequence of code points which all
-- | match the given predicate from the string. Operates in constant space and
-- | in time linear to the length of the string.
takeWhile :: (CodePoint -> Boolean) -> String -> String
takeWhile p s = take (count p s) s


-- | Creates an array of code points from a string. Operates in space and time
-- | linear to the length of the string.
toCodePointArray :: String -> Array CodePoint
toCodePointArray = _toCodePointArray toCodePointArrayFallback unsafeCodePointAt0

foreign import _toCodePointArray
  :: (String -> Array CodePoint)
  -> (String -> CodePoint)
  -> String
  -> Array CodePoint

toCodePointArrayFallback :: String -> Array CodePoint
toCodePointArrayFallback s = unfoldr unconsButWithTuple s

unconsButWithTuple :: String -> Maybe (Tuple CodePoint String)
unconsButWithTuple s = (\{ head, tail } -> Tuple head tail) <$> uncons s


-- | Returns a record with the first code point and the remaining code points
-- | of the string. Returns Nothing if the string is empty. Operates in
-- | constant space and time.
uncons :: String -> Maybe { head :: CodePoint, tail :: String }
uncons s = case String.length s of
  0 -> Nothing
  1 -> Just { head: CodePoint (Unsafe.charCodeAt 0 s), tail: "" }
  _ ->
    let cu0 = Unsafe.charCodeAt 0 s in
    let cu1 = Unsafe.charCodeAt 1 s in
    if isLead cu0 && isTrail cu1
      then Just { head: unsurrogate cu0 cu1, tail: String.drop 2 s }
      else Just { head: CodePoint cu0, tail: String.drop 1 s }

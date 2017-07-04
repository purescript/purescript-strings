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
-- WARN: This list must be updated to re-export any exports added to Data.String. That makes me sad.
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

codePointFromSurrogatePair :: Int -> Int -> Maybe CodePoint
codePointFromSurrogatePair lead trail | isLead lead && isTrail trail =
  Just (unsurrogate lead trail)
codePointFromSurrogatePair _ _ = Nothing

unsurrogate :: Int -> Int -> CodePoint
unsurrogate h l = CodePoint ((h - 0xD800) * 0x400 + (l - 0xDC00) + 0x10000)

isLead :: Int -> Boolean
isLead cu = 0xD800 <= cu && cu <= 0xDBFF

isTrail :: Int -> Boolean
isTrail cu = 0xDC00 <= cu && cu <= 0xDFFF

fromCharCode :: Int -> String
fromCharCode = String.singleton <<< Char.fromCharCode

unsafeCodePointAt0 :: String -> CodePoint
unsafeCodePointAt0 = _unsafeCodePointAt0 unsafeCodePointAt0Fallback

foreign import _unsafeCodePointAt0
  :: (String -> CodePoint)
  -> String
  -> CodePoint

unsafeCodePointAt0Fallback :: String -> CodePoint
unsafeCodePointAt0Fallback s | String.length s == 1 = CodePoint (Unsafe.charCodeAt 0 s)
unsafeCodePointAt0Fallback s = CodePoint (((lead - 0xD800) * 0x400) + (trail - 0xDC00) + 0x10000)
  where
    lead = Unsafe.charCodeAt 0 s
    trail = Unsafe.charCodeAt 1 s


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


count :: (CodePoint -> Boolean) -> String -> Int
count = _count isLead isTrail unsurrogate

foreign import _count
  :: (Int -> Boolean)
  -> (Int -> Boolean)
  -> (Int -> Int -> CodePoint)
  -> (CodePoint -> Boolean)
  -> String
  -> Int


drop :: Int -> String -> String
drop n s = fromCodePointArray (Array.drop n (toCodePointArray s))


dropWhile :: (CodePoint -> Boolean) -> String -> String
dropWhile p s = drop (count p s) s


fromCodePointArray :: Array CodePoint -> String
fromCodePointArray = _fromCodePointArray singletonFallback

foreign import _fromCodePointArray
  :: (CodePoint -> String)
  -> Array CodePoint
  -> String


indexOf :: String.Pattern -> String -> Maybe Int
indexOf p s = (\i -> length (String.take i s)) <$> String.indexOf p s


indexOf' :: String.Pattern -> Int -> String -> Maybe Int
indexOf' p i s =
  let s' = drop i s in
  (\k -> i + length (String.take k s')) <$> String.indexOf p s'


lastIndexOf :: String.Pattern -> String -> Maybe Int
lastIndexOf p s = (\i -> length (String.take i s)) <$> String.lastIndexOf p s


lastIndexOf' :: String.Pattern -> Int -> String -> Maybe Int
lastIndexOf' p i s =
  let s' = drop i s in
  (\k -> i + length (String.take k s')) <$> String.lastIndexOf p s'


length :: String -> Int
length = Array.length <<< toCodePointArray


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


splitAt :: Int -> String -> Maybe { before :: String, after :: String }
splitAt i s =
  let cps = toCodePointArray s in
  if i < 0 || Array.length cps < i
    then Nothing
    else Just {
        before: fromCodePointArray (Array.take i cps),
        after: fromCodePointArray (Array.drop i cps)
      }


take :: Int -> String -> String
take = _take takeFallback

foreign import _take :: (Int -> String -> String) -> Int -> String -> String

takeFallback :: Int -> String -> String
takeFallback n s = fromCodePointArray (Array.take n (toCodePointArray s))


takeWhile :: (CodePoint -> Boolean) -> String -> String
takeWhile p s = take (count p s) s


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
  decode (Cons h (Cons l rest)) | isLead h && isTrail l
    = Just (Tuple (unsurrogate h l) rest)
  decode (Cons c rest) = Just (Tuple (CodePoint c) rest)
  decode Nil = Nothing


uncons :: String -> Maybe { head :: CodePoint, tail :: String }
uncons s  = { head: _, tail: drop 1 s } <$> codePointAt 0 s

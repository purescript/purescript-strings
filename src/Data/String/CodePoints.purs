module CodePoints
  ( CodePoint()
  --, Pattern()
  , codePointAt
  , codePointFromInt
  , codePointToInt
  --, contains
  , count
  , drop
  --, dropWhile
  --, indexOf
  --, indexOf'
  --, lastIndexOf
  --, lastIndexOf'
  , length
  --, replace
  --, replaceAll
  , singleton
  --, split
  --, splitAt
  --, stripPrefix
  --, stripSuffix
  , take
  --, takeWhile
  --, uncons
  , toCodePointArray
  --, fromCodePointArray
  ) where

import Prelude ((&&), (*), (+), (-), (<$>), (<=), (<<<))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (toCharArray)
import Data.Unfoldable (unfoldr)
import Data.List (List(Cons, Nil), fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Data.Array as Array
import Data.Char (toCharCode)

newtype CodePoint = CodePoint Int

codePointFromInt :: Int -> Maybe CodePoint
codePointFromInt n | 0 <= n && n <= 0x10FFFF = Just (CodePoint n)
codePointFromInt n = Nothing

codePointToInt :: CodePoint -> Int
codePointToInt (CodePoint n) = n

codePointFromSurrogatePair :: Int -> Int -> Maybe CodePoint
codePointFromSurrogatePair lead trail | isLead lead && isTrail trail
  = Just (CodePoint (unsurrogate lead trail))
codePointFromSurrogatePair _ _ = Nothing

unsurrogate :: Int -> Int -> Int
unsurrogate h l = (h - 0xD800) * 0x400 + (l - 0xDC00) + 0x10000

isLead :: Int -> Boolean
isLead cu = 0xD800 <= cu && cu <= 0xDBFF

isTrail :: Int -> Boolean
isTrail cu = 0xDC00 <= cu && cu <= 0xDFFF


codePointAt :: Int -> String -> Maybe CodePoint
codePointAt = _codePointAt codePointAtFallback Just Nothing

foreign import _codePointAt
  :: (Int -> String -> Maybe CodePoint)
  -> (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Int
  -> String
  -> Maybe CodePoint

codePointAtFallback :: Int -> String -> Maybe CodePoint
codePointAtFallback n s = Array.index (toCodePointArray s) n


count :: (CodePoint -> Boolean) -> String -> Int
count pred = Array.length <<< Array.filter pred <<< toCodePointArray


drop :: Int -> String -> String
drop n s = fromCodePointArray (Array.drop n (toCodePointArray s))


length :: String -> Int
length = Array.length <<< toCodePointArray


foreign import singleton :: CodePoint -> String


take :: Int -> String -> String
take = _take takeFallback

foreign import _take :: (Int -> String -> String) -> Int -> String -> String

takeFallback :: Int -> String -> String
takeFallback n s = fromCodePointArray (Array.take n (toCodePointArray s))


toCodePointArray :: String -> Array CodePoint
toCodePointArray = _toCodePointArray toCodePointArrayFallback

foreign import _toCodePointArray
  :: (String -> Array CodePoint)
  -> String
  -> Array CodePoint

toCodePointArrayFallback :: String -> Array CodePoint
toCodePointArrayFallback s = unfoldr decode (fromFoldable (toCharCode <$> toCharArray s))
  where
  decode :: List Int -> Maybe (Tuple CodePoint (List Int))
  decode (Cons h (Cons l rest)) | isLead h && isTrail l
    = Just (Tuple (CodePoint (unsurrogate h l)) rest)
  decode (Cons c rest) = Just (Tuple (CodePoint c) rest)
  decode Nil = Nothing


foreign import fromCodePointArray :: Array CodePoint -> String

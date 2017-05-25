module CodePoints
  ( CodePoint()
  --, Pattern()
  --, codePointAt
  --, fromCodePointArray
  --, contains
  --, indexOf
  --, indexOf'
  --, lastIndexOf
  --, lastIndexOf'
  --, uncons
  --, length
  --, singleton
  --, replace
  --, replaceAll
  --, take
  --, takeWhile
  --, drop
  --, dropWhile
  --, stripPrefix
  --, stripSuffix
  --, count
  --, split
  --, splitAt
  --, toCodePointArray
  ) where

import Prelude ((&&), (<=), (*), (+), (-))
import Data.Maybe (Maybe(Just, Nothing))

newtype CodePoint = CodePoint Int

codePointFromInt :: Int -> Maybe CodePoint
codePointFromInt n | 0 <= n && n <= 0x10FFFF = Just (CodePoint n)
codePointFromInt n = Nothing

codePointToInt :: CodePoint -> Int
codePointToInt (CodePoint n) = n

codePointFromSurrogatePair :: Int -> Int -> Maybe CodePoint
codePointFromSurrogatePair lead trail | isLead lead && isTrail trail
  = Just (CodePoint (unsurrogate lead trail))
    where unsurrogate h l = (h - 0xD800) * 0x400 + (l - 0xDC00) + 0x10000
codePointFromSurrogatePair _ _ = Nothing

isLead :: Int -> Boolean
isLead cu = 0xD800 <= cu && cu <= 0xDBFF

isTrail :: Int -> Boolean
isTrail cu = 0xDC00 <= cu && cu <= 0xDFFF

codePointAt :: Int -> String -> Maybe CodePoint
codePointAt = _codePointAt (Just . CodePoint) Nothing

foreign import _codePointAt
  :: (Int -> String -> Maybe CodePoint)
  -> (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Int
  -> String
  -> Maybe CodePoint

codePointAtFallback :: Int -> String -> Maybe CodePoint
codePointAtFallback n s = CodePoint <$> index (toCodePointArray s) n

foreign import _toCodePointArray :: String -> Array CodePoint

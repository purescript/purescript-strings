module Data.String.NonEmpty
  ( NonEmptyString(..)
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.String (uncons, singleton) as Str
-- | A non-empty String
-- |
-- | For example:
-- |
-- | ```purescript
-- | nonEmptyString :: NonEmptyString
-- | nonEmptyString = NonEmptyString 'C' "char"
-- | ```
data NonEmptyString = NonEmptyString Char String

singleton :: Char -> NonEmptyString
singleton c = NonEmptyString c ""

fromString :: String -> Maybe NonEmptyString
fromString = Str.uncons >>> map \{head, tail} -> NonEmptyString head tail

toString :: NonEmptyString -> String
toString (NonEmptyString c s) = Str.singleton c <> s

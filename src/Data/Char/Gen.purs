module Data.Char.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.Char as C

-- | Generates a character of the Unicode basic multilingual plain.
genUnicodeChar :: forall m. MonadGen m => m Char
genUnicodeChar = C.fromCharCode <$> chooseInt 0 65536

-- | Generates a character in the ASCII character set, excluding control codes.
genAsciiChar :: forall m. MonadGen m => m Char
genAsciiChar = C.fromCharCode <$> chooseInt 32 127

-- | Generates a character in the ASCII character set.
genAsciiChar' :: forall m. MonadGen m => m Char
genAsciiChar' = C.fromCharCode <$> chooseInt 0 127

-- | Generates a character that is a numeric digit.
genDigitChar :: forall m. MonadGen m => m Char
genDigitChar = C.fromCharCode <$> chooseInt 48 57

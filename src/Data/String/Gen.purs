module Data.String.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt, unfoldable, sized, resize)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char.Gen as CG
import Data.String as S

-- | Generates a string using the specified character generator.
genString :: forall m. MonadRec m => MonadGen m => m Char -> m String
genString genChar = sized \size -> do
  newSize <- chooseInt 1 (max 1 size)
  resize (const newSize) $ S.fromCharArray <$> unfoldable genChar

-- | Generates a string using characters from the Unicode basic multilingual
-- | plain.
genUnicodeString :: forall m. MonadRec m => MonadGen m => m String
genUnicodeString = genString CG.genUnicodeChar

-- | Generates a string using the ASCII character set, excluding control codes.
genAsciiString :: forall m. MonadRec m => MonadGen m => m String
genAsciiString = genString CG.genAsciiChar

-- | Generates a string using the ASCII character set.
genAsciiString' :: forall m. MonadRec m => MonadGen m => m String
genAsciiString' = genString CG.genAsciiChar'

-- | Generates a string made up of numeric digits.
genDigitString :: forall m. MonadRec m => MonadGen m => m String
genDigitString = genString CG.genDigitChar

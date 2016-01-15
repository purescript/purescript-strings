
module Data.Char.Unicode where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.Char.Internal
import Data.Maybe

-- | Unicode General Categories (column 2 of the UnicodeData table) in
-- the order they are listed in the Unicode standard (the Unicode
-- Character Database, in particular).
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> :t OtherLetter
-- OtherLetter :: GeneralCategory
--
-- 'Eq' instance:
--
-- >>> UppercaseLetter == UppercaseLetter
-- True
-- >>> UppercaseLetter == LowercaseLetter
-- False
--
-- 'Ord' instance:
--
-- >>> NonSpacingMark <= MathSymbol
-- True
--
-- 'Enum' instance:
--
-- >>> enumFromTo ModifierLetter SpacingCombiningMark
-- [ModifierLetter,OtherLetter,NonSpacingMark,SpacingCombiningMark]
--
-- 'Read' instance:
--
-- >>> read "DashPunctuation" :: GeneralCategory
-- DashPunctuation
-- >>> read "17" :: GeneralCategory
-- *** Exception: Prelude.read: no parse
--
-- 'Show' instance:
--
-- >>> show EnclosingMark
-- "EnclosingMark"
--
-- 'Bounded' instance:
--
-- >>> minBound :: GeneralCategory
-- UppercaseLetter
-- >>> maxBound :: GeneralCategory
-- NotAssigned
--
-- 'Ix' instance:
--
--  >>> import Data.Ix ( index )
--  >>> index (OtherLetter,Control) FinalQuote
--  12
--  >>> index (OtherLetter,Control) Format
--  *** Exception: Error in array index
data GeneralCategory
        = UppercaseLetter       -- ^ Lu: Letter, Uppercase
        | LowercaseLetter       -- ^ Ll: Letter, Lowercase
        | TitlecaseLetter       -- ^ Lt: Letter, Titlecase
        | ModifierLetter        -- ^ Lm: Letter, Modifier
        | OtherLetter           -- ^ Lo: Letter, Other
        | NonSpacingMark        -- ^ Mn: Mark, Non-Spacing
        | SpacingCombiningMark  -- ^ Mc: Mark, Spacing Combining
        | EnclosingMark         -- ^ Me: Mark, Enclosing
        | DecimalNumber         -- ^ Nd: Number, Decimal
        | LetterNumber          -- ^ Nl: Number, Letter
        | OtherNumber           -- ^ No: Number, Other
        | ConnectorPunctuation  -- ^ Pc: Punctuation, Connector
        | DashPunctuation       -- ^ Pd: Punctuation, Dash
        | OpenPunctuation       -- ^ Ps: Punctuation, Open
        | ClosePunctuation      -- ^ Pe: Punctuation, Close
        | InitialQuote          -- ^ Pi: Punctuation, Initial quote
        | FinalQuote            -- ^ Pf: Punctuation, Final quote
        | OtherPunctuation      -- ^ Po: Punctuation, Other
        | MathSymbol            -- ^ Sm: Symbol, Math
        | CurrencySymbol        -- ^ Sc: Symbol, Currency
        | ModifierSymbol        -- ^ Sk: Symbol, Modifier
        | OtherSymbol           -- ^ So: Symbol, Other
        | Space                 -- ^ Zs: Separator, Space
        | LineSeparator         -- ^ Zl: Separator, Line
        | ParagraphSeparator    -- ^ Zp: Separator, Paragraph
        | Control               -- ^ Cc: Other, Control
        | Format                -- ^ Cf: Other, Format
        | Surrogate             -- ^ Cs: Other, Surrogate
        | PrivateUse            -- ^ Co: Other, Private Use
        | NotAssigned           -- ^ Cn: Other, Not Assigned
-- deriving (Show, Eq, Ord, Enum, Bounded, Ix)

generalCatToInt :: GeneralCategory -> Int
generalCatToInt UppercaseLetter = 1
generalCatToInt LowercaseLetter = 2
generalCatToInt TitlecaseLetter = 3
generalCatToInt ModifierLetter = 4
generalCatToInt OtherLetter = 5
generalCatToInt NonSpacingMark = 6
generalCatToInt SpacingCombiningMark = 7
generalCatToInt EnclosingMark = 8
generalCatToInt DecimalNumber = 9
generalCatToInt LetterNumber = 10
generalCatToInt OtherNumber = 11
generalCatToInt ConnectorPunctuation = 12
generalCatToInt DashPunctuation = 13
generalCatToInt OpenPunctuation = 14
generalCatToInt ClosePunctuation = 15
generalCatToInt InitialQuote = 16
generalCatToInt FinalQuote = 17
generalCatToInt OtherPunctuation = 18
generalCatToInt MathSymbol = 19
generalCatToInt CurrencySymbol = 20
generalCatToInt ModifierSymbol = 21
generalCatToInt OtherSymbol = 22
generalCatToInt Space = 23
generalCatToInt LineSeparator = 24
generalCatToInt ParagraphSeparator = 25
generalCatToInt Control = 26
generalCatToInt Format = 27
generalCatToInt Surrogate = 28
generalCatToInt PrivateUse = 29
generalCatToInt NotAssigned = 30

generalCatToUnicodeCat :: GeneralCategory -> UnicodeCategory
generalCatToUnicodeCat UppercaseLetter = NUMCAT_LU
generalCatToUnicodeCat LowercaseLetter = NUMCAT_LL
generalCatToUnicodeCat TitlecaseLetter = NUMCAT_LT
generalCatToUnicodeCat ModifierLetter = NUMCAT_LM
generalCatToUnicodeCat OtherLetter = NUMCAT_LO
generalCatToUnicodeCat NonSpacingMark = NUMCAT_MN
generalCatToUnicodeCat SpacingCombiningMark = NUMCAT_MC
generalCatToUnicodeCat EnclosingMark = NUMCAT_ME
generalCatToUnicodeCat DecimalNumber = NUMCAT_ND
generalCatToUnicodeCat LetterNumber = NUMCAT_NL
generalCatToUnicodeCat OtherNumber = NUMCAT_NO
generalCatToUnicodeCat ConnectorPunctuation = NUMCAT_PC
generalCatToUnicodeCat DashPunctuation = NUMCAT_PD
generalCatToUnicodeCat OpenPunctuation = NUMCAT_PS
generalCatToUnicodeCat ClosePunctuation = NUMCAT_PE
generalCatToUnicodeCat InitialQuote = NUMCAT_PI
generalCatToUnicodeCat FinalQuote = NUMCAT_PF
generalCatToUnicodeCat OtherPunctuation = NUMCAT_PO
generalCatToUnicodeCat MathSymbol = NUMCAT_SM
generalCatToUnicodeCat CurrencySymbol = NUMCAT_SC
generalCatToUnicodeCat ModifierSymbol = NUMCAT_SK
generalCatToUnicodeCat OtherSymbol = NUMCAT_SO
generalCatToUnicodeCat Space = NUMCAT_ZS
generalCatToUnicodeCat LineSeparator = NUMCAT_ZL
generalCatToUnicodeCat ParagraphSeparator = NUMCAT_ZP
generalCatToUnicodeCat Control = NUMCAT_CC
generalCatToUnicodeCat Format = NUMCAT_CF
generalCatToUnicodeCat Surrogate = NUMCAT_CS
generalCatToUnicodeCat PrivateUse = NUMCAT_CO
generalCatToUnicodeCat NotAssigned = NUMCAT_CN

unicodeCatToGeneralCat :: UnicodeCategory -> GeneralCategory
unicodeCatToGeneralCat NUMCAT_LU = UppercaseLetter
unicodeCatToGeneralCat NUMCAT_LL = LowercaseLetter
unicodeCatToGeneralCat NUMCAT_LT = TitlecaseLetter
unicodeCatToGeneralCat NUMCAT_LM = ModifierLetter
unicodeCatToGeneralCat NUMCAT_LO = OtherLetter
unicodeCatToGeneralCat NUMCAT_MN = NonSpacingMark
unicodeCatToGeneralCat NUMCAT_MC = SpacingCombiningMark
unicodeCatToGeneralCat NUMCAT_ME = EnclosingMark
unicodeCatToGeneralCat NUMCAT_ND = DecimalNumber
unicodeCatToGeneralCat NUMCAT_NL = LetterNumber
unicodeCatToGeneralCat NUMCAT_NO = OtherNumber
unicodeCatToGeneralCat NUMCAT_PC = ConnectorPunctuation
unicodeCatToGeneralCat NUMCAT_PD = DashPunctuation
unicodeCatToGeneralCat NUMCAT_PS = OpenPunctuation
unicodeCatToGeneralCat NUMCAT_PE = ClosePunctuation
unicodeCatToGeneralCat NUMCAT_PI = InitialQuote
unicodeCatToGeneralCat NUMCAT_PF = FinalQuote
unicodeCatToGeneralCat NUMCAT_PO = OtherPunctuation
unicodeCatToGeneralCat NUMCAT_SM = MathSymbol
unicodeCatToGeneralCat NUMCAT_SC = CurrencySymbol
unicodeCatToGeneralCat NUMCAT_SK = ModifierSymbol
unicodeCatToGeneralCat NUMCAT_SO = OtherSymbol
unicodeCatToGeneralCat NUMCAT_ZS = Space
unicodeCatToGeneralCat NUMCAT_ZL = LineSeparator
unicodeCatToGeneralCat NUMCAT_ZP = ParagraphSeparator
unicodeCatToGeneralCat NUMCAT_CC = Control
unicodeCatToGeneralCat NUMCAT_CF = Format
unicodeCatToGeneralCat NUMCAT_CS = Surrogate
unicodeCatToGeneralCat NUMCAT_CO = PrivateUse
unicodeCatToGeneralCat NUMCAT_CN = NotAssigned

instance showGeneralCategory :: Show GeneralCategory where
    show UppercaseLetter = "UppercaseLetter"
    show LowercaseLetter = "LowercaseLetter"
    show TitlecaseLetter = "TitlecaseLetter"
    show ModifierLetter = "ModifierLetter"
    show OtherLetter = "OtherLetter"
    show NonSpacingMark = "NonSpacingMark"
    show SpacingCombiningMark = "SpacingCombiningMark"
    show EnclosingMark = "EnclosingMark"
    show DecimalNumber = "DecimalNumber"
    show LetterNumber = "LetterNumber"
    show OtherNumber = "OtherNumber"
    show ConnectorPunctuation = "ConnectorPunctuation"
    show DashPunctuation = "DashPunctuation"
    show OpenPunctuation = "OpenPunctuation"
    show ClosePunctuation = "ClosePunctuation"
    show InitialQuote = "InitialQuote"
    show FinalQuote = "FinalQuote"
    show OtherPunctuation = "OtherPunctuation"
    show MathSymbol = "MathSymbol"
    show CurrencySymbol = "CurrencySymbol"
    show ModifierSymbol = "ModifierSymbol"
    show OtherSymbol = "OtherSymbol"
    show Space = "Space"
    show LineSeparator = "LineSeparator"
    show ParagraphSeparator = "ParagraphSeparator"
    show Control = "Control"
    show Format = "Format"
    show Surrogate = "Surrogate"
    show PrivateUse = "PrivateUse"
    show NotAssigned = "NotAssigned"

instance eqGeneralCategory :: Eq GeneralCategory where
    eq UppercaseLetter UppercaseLetter = true
    eq LowercaseLetter LowercaseLetter = true
    eq TitlecaseLetter TitlecaseLetter = true
    eq ModifierLetter ModifierLetter = true
    eq OtherLetter OtherLetter = true
    eq NonSpacingMark NonSpacingMark = true
    eq SpacingCombiningMark SpacingCombiningMark = true
    eq EnclosingMark EnclosingMark = true
    eq DecimalNumber DecimalNumber = true
    eq LetterNumber LetterNumber = true
    eq OtherNumber OtherNumber = true
    eq ConnectorPunctuation ConnectorPunctuation = true
    eq DashPunctuation DashPunctuation = true
    eq OpenPunctuation OpenPunctuation = true
    eq ClosePunctuation ClosePunctuation = true
    eq InitialQuote InitialQuote = true
    eq FinalQuote FinalQuote = true
    eq OtherPunctuation OtherPunctuation = true
    eq MathSymbol MathSymbol = true
    eq CurrencySymbol CurrencySymbol = true
    eq ModifierSymbol ModifierSymbol = true
    eq OtherSymbol OtherSymbol = true
    eq Space Space = true
    eq LineSeparator LineSeparator = true
    eq ParagraphSeparator ParagraphSeparator = true
    eq Control Control = true
    eq Format Format = true
    eq Surrogate Surrogate = true
    eq PrivateUse PrivateUse = true
    eq NotAssigned NotAssigned = true
    eq _ _ = false

instance ordGeneralCategory :: Ord GeneralCategory where
    compare catA catB = compare (generalCatToInt catA) (generalCatToInt catB)

instance boundedGeneralCategory :: Bounded GeneralCategory where
    bottom = UppercaseLetter
    top = NotAssigned

-- -- | The Unicode general category of the character. This relies on the
-- -- 'Enum' instance of 'GeneralCategory', which must remain in the
-- -- same order as the categories are presented in the Unicode
-- -- standard.
-- --
-- -- ==== __Examples__
-- --
-- -- Basic usage:
-- --
-- -- >>> generalCategory 'a'
-- -- LowercaseLetter
-- -- >>> generalCategory 'A'
-- -- UppercaseLetter
-- -- >>> generalCategory '0'
-- -- DecimalNumber
-- -- >>> generalCategory '%'
-- -- OtherPunctuation
-- -- >>> generalCategory '♥'
-- -- OtherSymbol
-- -- >>> generalCategory '\31'
-- -- Control
-- -- >>> generalCategory ' '
-- -- Space
-- --
generalCategory :: Char -> Maybe GeneralCategory
generalCategory = map unicodeCatToGeneralCat <<< uGencat <<< toCharCode

-- | Selects the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.
isAscii :: Char -> Boolean
isAscii c =  c <  '\x80'

-- | Selects the first 256 characters of the Unicode character set,
-- corresponding to the ISO 8859-1 (Latin-1) character set.
isLatin1 :: Char -> Boolean
isLatin1 c =  c <= '\xff'

-- | Selects ASCII lower-case letters,
-- i.e. characters satisfying both 'isAscii' and 'isLower'.
isAsciiLower :: Char -> Boolean
isAsciiLower c =  c >= 'a' && c <= 'z'

-- | Selects ASCII upper-case letters,
-- i.e. characters satisfying both 'isAscii' and 'isUpper'.
isAsciiUpper :: Char -> Boolean
isAsciiUpper c =  c >= 'A' && c <= 'Z'

-- | Selects control characters, which are the non-printing characters of
-- the Latin-1 subset of Unicode.
isControl :: Char -> Boolean
isControl = uIswcntrl <<< toCharCode

-- | Selects printable Unicode characters
-- (letters, numbers, marks, punctuation, symbols and spaces).
isPrint :: Char -> Boolean
isPrint = uIswprint <<< toCharCode

-- | Returns 'True' for any Unicode space character, and the control
-- characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.
isSpace :: Char -> Boolean
-- isSpace includes non-breaking space
-- The magic 0x377 isn't really that magical. As of 2014, all the codepoints
-- at or below 0x377 have been assigned, so we shouldn't have to worry about
-- any new spaces appearing below there. It would probably be best to
-- use branchless ||, but currently the eqLit transformation will undo that,
-- so we'll do it like this until there's a way around that.
isSpace c = if uc <= 0x337
               then uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
               else uIswspace $ toCharCode c
  where
    uc :: Int
    uc = toCharCode c

-- | Selects upper-case or title-case alphabetic Unicode characters (letters).
-- Title case is used by a small number of letter ligatures like the
-- single-character form of /Lj/.
isUpper :: Char -> Boolean
isUpper = uIswupper <<< toCharCode

-- | Selects lower-case alphabetic Unicode characters (letters).
isLower :: Char -> Boolean
isLower = uIswlower <<< toCharCode

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- title-case letters, plus letters of caseless scripts and modifiers letters).
-- This function is equivalent to 'Data.Char.isLetter'.
isAlpha :: Char -> Boolean
isAlpha = uIswalpha <<< toCharCode

-- | Selects alphabetic or numeric digit Unicode characters.
--
-- Note that numeric digits outside the ASCII range are selected by this
-- function but not by 'isDigit'.  Such digits may be part of identifiers
-- but are not used by the printer and reader to represent numbers.
isAlphaNum :: Char -> Boolean
isAlphaNum = uIswalnum <<< toCharCode

-- | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit :: Char -> Boolean
isDigit c = (toCharCode c - toCharCode '0') <= 9

-- We use an addition and an unsigned comparison instead of two signed
-- comparisons because it's usually faster and puts less strain on branch
-- prediction. It likely also enables some CSE when combined with functions
-- that follow up with an actual conversion.

-- | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
isOctDigit :: Char -> Boolean
isOctDigit c = (toCharCode c - toCharCode '0') <= 7

-- | Selects ASCII hexadecimal digits,
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
isHexDigit :: Char -> Boolean
isHexDigit c = isDigit c
            || (toCharCode c - toCharCode 'A') <= 5
            || (toCharCode c - toCharCode 'a') <= 5

-- | Selects Unicode punctuation characters, including various kinds
-- of connectors, brackets and quotes.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'ConnectorPunctuation'
-- * 'DashPunctuation'
-- * 'OpenPunctuation'
-- * 'ClosePunctuation'
-- * 'InitialQuote'
-- * 'FinalQuote'
-- * 'OtherPunctuation'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Punctuation\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isPunctuation 'a'
-- False
-- >>> isPunctuation '7'
-- False
-- >>> isPunctuation '♥'
-- False
-- >>> isPunctuation '"'
-- True
-- >>> isPunctuation '?'
-- True
-- >>> isPunctuation '—'
-- True
--
isPunctuation :: Char -> Boolean
isPunctuation c =
    case generalCategory c of
        Just ConnectorPunctuation    -> true
        Just DashPunctuation         -> true
        Just OpenPunctuation         -> true
        Just ClosePunctuation        -> true
        Just InitialQuote            -> true
        Just FinalQuote              -> true
        Just OtherPunctuation        -> true
        _                            -> false

-- | Selects Unicode symbol characters, including mathematical and
-- currency symbols.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'MathSymbol'
-- * 'CurrencySymbol'
-- * 'ModifierSymbol'
-- * 'OtherSymbol'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Symbol\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isSymbol 'a'
-- False
-- >>> isSymbol '6'
-- False
-- >>> isSymbol '='
-- True
--
-- The definition of \"math symbol\" may be a little
-- counter-intuitive depending on one's background:
--
-- >>> isSymbol '+'
-- True
-- >>> isSymbol '-'
-- False
--
isSymbol :: Char -> Boolean
isSymbol c =
    case generalCategory c of
        Just MathSymbol              -> true
        Just CurrencySymbol          -> true
        Just ModifierSymbol          -> true
        Just OtherSymbol             -> true
        _                            -> false

-- | Convert a letter to the corresponding upper-case letter, if any.
-- Any other character is returned unchanged.
toUpper :: Char -> Char
toUpper = fromCharCode <<< uTowupper <<< toCharCode

-- | Convert a letter to the corresponding lower-case letter, if any.
-- Any other character is returned unchanged.
toLower :: Char -> Char
toLower = fromCharCode <<< uTowlower <<< toCharCode

-- | Convert a letter to the corresponding title-case or upper-case
-- letter, if any.  (Title case differs from upper case only for a small
-- number of ligature letters.)
-- Any other character is returned unchanged.
toTitle :: Char -> Char
toTitle = fromCharCode <<< uTowtitle <<< toCharCode

-- | Convert a single digit 'Char' to the corresponding 'Int'.  This
-- function fails unless its argument satisfies 'isHexDigit', but
-- recognises both upper- and lower-case hexadecimal digits (that
-- is, @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@).
--
-- ==== __Examples__
--
-- Characters @\'0\'@ through @\'9\'@ are converted properly to
-- @0..9@:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
--
-- Both upper- and lower-case @\'A\'@ through @\'F\'@ are converted
-- as well, to @10..15@.
--
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]
--
-- Anything else throws an exception:
--
-- >>> digitToInt 'G'
-- *** Exception: Char.digitToInt: not a digit 'G'
-- >>> digitToInt '♥'
-- *** Exception: Char.digitToInt: not a digit '\9829'
--
-- TODO: This function.
--
-- digitToInt :: Char -> Int
-- digitToInt c
--   | (fromIntegral dec::Word) <= 9 = dec
--   | (fromIntegral hexl::Word) <= 5 = hexl + 10
--   | (fromIntegral hexu::Word) <= 5 = hexu + 10
--   | otherwise = errorWithoutStackTrace ("Char.digitToInt: not a digit " ++ show c) -- sigh
--   where
--     dec = ord c - ord '0'
--     hexl = ord c - ord 'a'
--     hexu = ord c - ord 'A'

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- title-case letters, plus letters of caseless scripts and
-- modifiers letters). This function is equivalent to
-- 'Data.Char.isAlpha'.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'UppercaseLetter'
-- * 'LowercaseLetter'
-- * 'TitlecaseLetter'
-- * 'ModifierLetter'
-- * 'OtherLetter'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Letter\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isLetter 'a'
-- True
-- >>> isLetter 'A'
-- True
-- >>> isLetter '0'
-- False
-- >>> isLetter '%'
-- False
-- >>> isLetter '♥'
-- False
-- >>> isLetter '\31'
-- False
--
-- Ensure that 'isLetter' and 'isAlpha' are equivalent.
--
-- >>> let chars = [(chr 0)..]
-- >>> let letters = map isLetter chars
-- >>> let alphas = map isAlpha chars
-- >>> letters == alphas
-- True
--
isLetter :: Char -> Boolean
isLetter c =
    case generalCategory c of
        Just UppercaseLetter         -> true
        Just LowercaseLetter         -> true
        Just TitlecaseLetter         -> true
        Just ModifierLetter          -> true
        Just OtherLetter             -> true
        _                            -> false

-- | Selects Unicode mark characters, for example accents and the
-- like, which combine with preceding characters.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'NonSpacingMark'
-- * 'SpacingCombiningMark'
-- * 'EnclosingMark'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Mark\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isMark 'a'
-- False
-- >>> isMark '0'
-- False
--
-- Combining marks such as accent characters usually need to follow
-- another character before they become printable:
--
-- >>> map isMark "ò"
-- [False,True]
--
-- Puns are not necessarily supported:
--
-- >>> isMark '✓'
-- False
--
isMark :: Char -> Boolean
isMark c =
    case generalCategory c of
        Just NonSpacingMark          -> true
        Just SpacingCombiningMark    -> true
        Just EnclosingMark           -> true
        _                            -> false

-- | Selects Unicode numeric characters, including digits from various
-- scripts, Roman numerals, et cetera.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'DecimalNumber'
-- * 'LetterNumber'
-- * 'OtherNumber'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Number\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isNumber 'a'
-- False
-- >>> isNumber '%'
-- False
-- >>> isNumber '3'
-- True
--
-- ASCII @\'0\'@ through @\'9\'@ are all numbers:
--
-- >>> and $ map isNumber ['0'..'9']
-- True
--
-- Unicode Roman numerals are \"numbers\" as well:
--
-- >>> isNumber 'Ⅸ'
-- True
--
isNumber :: Char -> Boolean
isNumber c =
    case generalCategory c of
        Just DecimalNumber           -> true
        Just LetterNumber            -> true
        Just OtherNumber             -> true
        _                            -> false

-- | Selects Unicode space and separator characters.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'Space'
-- * 'LineSeparator'
-- * 'ParagraphSeparator'
--
-- These classes are defined in the
-- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table Unicode Character Database>,
-- part of the Unicode standard. The same document defines what is
-- and is not a \"Separator\".
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isSeparator 'a'
-- False
-- >>> isSeparator '6'
-- False
-- >>> isSeparator ' '
-- True
--
-- Warning: newlines and tab characters are not considered
-- separators.
--
-- >>> isSeparator '\n'
-- False
-- >>> isSeparator '\t'
-- False
--
-- But some more exotic characters are (like HTML's @&nbsp;@):
--
-- >>> isSeparator '\160'
-- True
--
isSeparator :: Char -> Boolean
isSeparator c =
    case generalCategory c of
        Just Space                   -> true
        Just LineSeparator           -> true
        Just ParagraphSeparator      -> true
        _                            -> false


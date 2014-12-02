# Module Documentation

## Module Data.Char

### Types

    newtype Char


### Type Class Instances

    instance eqChar :: Eq Char

    instance ordChar :: Ord Char

    instance showChar :: Show Char


### Values

    charInRange :: Char -> Char -> Char -> Boolean

    charString :: Char -> String

    fromCharCode :: Number -> Char

    isDigit :: Char -> Boolean

    isHexDigit :: Char -> Boolean

    isOctDigit :: Char -> Boolean

    isSpace :: Char -> Boolean

    toCharCode :: Char -> Number


## Module Data.String

### Values

    charAt :: Number -> String -> Maybe Char

    charCodeAt :: Number -> String -> Maybe Number

    count :: (Char -> Boolean) -> String -> Number

    drop :: Number -> String -> String

    dropWhile :: (Char -> Boolean) -> String -> String

    fromChar :: Char -> String

    fromCharArray :: [Char] -> String

    indexOf :: String -> String -> Number

    indexOf' :: String -> Number -> String -> Number

    joinWith :: String -> [String] -> String

    lastIndexOf :: String -> String -> Number

    lastIndexOf' :: String -> Number -> String -> Number

    length :: String -> Number

    localeCompare :: String -> String -> Number

    null :: String -> Boolean

    replace :: String -> String -> String -> String

    reverse :: String -> String

    singleton :: Char -> String

    split :: String -> String -> [String]

    startsWith :: String -> String -> Boolean

    take :: Number -> String -> String

    takeWhile :: (Char -> Boolean) -> String -> String

    toCharArray :: String -> [Char]

    toLower :: String -> String

    toUpper :: String -> String

    trim :: String -> String

    trimLeft :: String -> String

    trimRight :: String -> String

    uncons :: String -> Maybe { tail :: String, head :: Char }


## Module Data.String.Regex

### Types

    data Regex :: *

    type RegexFlags = { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean }


### Type Class Instances

    instance showRegex :: Show Regex


### Values

    flags :: Regex -> RegexFlags

    match :: Regex -> String -> Maybe [String]

    parseFlags :: String -> RegexFlags

    regex :: String -> RegexFlags -> Regex

    renderFlags :: RegexFlags -> String

    replace :: Regex -> String -> String -> String

    replace' :: Regex -> (String -> [String] -> String) -> String -> String

    search :: Regex -> String -> Number

    source :: Regex -> String

    split :: Regex -> String -> [String]

    test :: Regex -> String -> Boolean


## Module Data.String.Unsafe

### Values

    charAt :: Number -> String -> Char

    charCodeAt :: Number -> String -> Number
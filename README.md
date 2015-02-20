# Module Documentation

## Module Data.Char

### Types


    newtype Char


### Type Class Instances


    instance eqChar :: Eq Char


    instance ordChar :: Ord Char


    instance showChar :: Show Char


### Values


    charString :: Char -> String


    fromCharCode :: Number -> Char


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


    singleton :: Char -> String


    split :: String -> String -> [String]


    take :: Number -> String -> String


    takeWhile :: (Char -> Boolean) -> String -> String


    toCharArray :: String -> [Char]


    toLower :: String -> String


    toUpper :: String -> String


    trim :: String -> String


    uncons :: String -> Maybe { tail :: String, head :: Char }


## Module Data.String.Regex

### Types


    data Regex :: *


    newtype RegexFlags where
      RegexFlags :: { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean } -> RegexFlags


### Type Class Instances


    instance regexFlagsBoolLike :: BoolLike RegexFlags

     | Example usage:
     | `regex "Foo" $ global <> ignoreCase`
     | is equivalent to
     | `/Foo/ig`

    instance regexFlagsSemiGroup :: Semigroup RegexFlags


    instance showRegex :: Show Regex


### Values


    flags :: Regex -> RegexFlags

     | Flags where `global : true` and all others are false

    global :: RegexFlags

     | Flags where `ignoreCase : true` and all others are false

    ignoreCase :: RegexFlags


    match :: Regex -> String -> Maybe [String]

     | Flags where `multiline : true` and all others are false

    multiline :: RegexFlags

     | Produce a new `RegexFlags` from `Booleans`

    newRegexFlags :: Boolean -> Boolean -> Boolean -> Boolean -> Boolean -> RegexFlags

     | All flags are set to false. Useful as a base for building flags or a default.

    noFlags :: RegexFlags

     | Perform a `Boolean` comparison across `RegexFlags`

    onRegexFlags :: (Boolean -> Boolean -> Boolean) -> RegexFlags -> RegexFlags -> RegexFlags


    parseFlags :: String -> RegexFlags


    regex :: String -> RegexFlags -> Regex


    renderFlags :: RegexFlags -> String


    replace :: Regex -> String -> String -> String


    replace' :: Regex -> (String -> [String] -> String) -> String -> String

     | Unwrap `RegexFlags` type to the underlying record

    runRegexFlags :: RegexFlags -> { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean }


    search :: Regex -> String -> Number


    source :: Regex -> String


    split :: Regex -> String -> [String]

     | Flags where `sticky : true` and all others are false

    sticky :: RegexFlags


    test :: Regex -> String -> Boolean

     | Flags where `unicode : true` and all others are false

    unicode :: RegexFlags


## Module Data.String.Unsafe

### Values


    charAt :: Number -> String -> Char


    charCodeAt :: Number -> String -> Number
# Module Documentation

## Module Data.Char

### Types

#### `Char`

    newtype Char


### Type Class Instances

#### `eqChar`

    instance eqChar :: Eq Char

#### `ordChar`

    instance ordChar :: Ord Char

#### `showChar`

    instance showChar :: Show Char


### Values

#### `charString`

    charString :: Char -> String

#### `fromCharCode`

    fromCharCode :: Number -> Char

#### `toCharCode`

    toCharCode :: Char -> Number


## Module Data.String

### Values

#### `charAt`

    charAt :: Number -> String -> Maybe Char

#### `charCodeAt`

    charCodeAt :: Number -> String -> Maybe Number

#### `count`

    count :: (Char -> Boolean) -> String -> Number

#### `drop`

    drop :: Number -> String -> String

#### `dropWhile`

    dropWhile :: (Char -> Boolean) -> String -> String

#### `fromChar`

    fromChar :: Char -> String

#### `fromCharArray`

    fromCharArray :: [Char] -> String

#### `indexOf`

    indexOf :: String -> String -> Number

#### `indexOf'`

    indexOf' :: String -> Number -> String -> Number

#### `joinWith`

    joinWith :: String -> [String] -> String

#### `lastIndexOf`

    lastIndexOf :: String -> String -> Number

#### `lastIndexOf'`

    lastIndexOf' :: String -> Number -> String -> Number

#### `length`

    length :: String -> Number

#### `localeCompare`

    localeCompare :: String -> String -> Number

#### `null`

    null :: String -> Boolean

#### `replace`

    replace :: String -> String -> String -> String

#### `singleton`

    singleton :: Char -> String

#### `split`

    split :: String -> String -> [String]

#### `take`

    take :: Number -> String -> String

#### `takeWhile`

    takeWhile :: (Char -> Boolean) -> String -> String

#### `toCharArray`

    toCharArray :: String -> [Char]

#### `toLower`

    toLower :: String -> String

#### `toUpper`

    toUpper :: String -> String

#### `trim`

    trim :: String -> String

#### `uncons`

    uncons :: String -> Maybe { tail :: String, head :: Char }


## Module Data.String.Regex

### Types

#### `Regex`

    data Regex :: *

#### `RegexFlags`

    newtype RegexFlags
      = RegexFlags { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean }


### Type Class Instances

#### `regexFlagsBoolLike`

    instance regexFlagsBoolLike :: BoolLike RegexFlags

#### `regexFlagsSemiGroup`

Example usage:
`regex "Foo" $ global <> ignoreCase`
is equivalent to
`/Foo/ig`

    instance regexFlagsSemiGroup :: Semigroup RegexFlags

#### `showRegex`

    instance showRegex :: Show Regex


### Values

#### `flags`

    flags :: Regex -> RegexFlags

#### `global`

Flags where `global : true` and all others are false

    global :: RegexFlags

#### `ignoreCase`

     Flags where `ignoreCase : true` and all others are false

    ignoreCase :: RegexFlags

#### `match`

    match :: Regex -> String -> Maybe [String]

#### `multiline`

Flags where `multiline : true` and all others are false

    multiline :: RegexFlags

#### `newRegexFlags`

Produce a new `RegexFlags` from `Booleans`

    newRegexFlags :: Boolean -> Boolean -> Boolean -> Boolean -> Boolean -> RegexFlags

#### `noFlags`

All flags are set to false. Useful as a base for building flags or a default.

    noFlags :: RegexFlags

#### `onRegexFlags`

Perform a `Boolean` comparison across `RegexFlags`

    onRegexFlags :: (Boolean -> Boolean -> Boolean) -> RegexFlags -> RegexFlags -> RegexFlags

#### `parseFlags`

    parseFlags :: String -> RegexFlags

#### `regex`

    regex :: String -> RegexFlags -> Regex

#### `renderFlags`

    renderFlags :: RegexFlags -> String

#### `replace`

    replace :: Regex -> String -> String -> String

#### `replace'`

    replace' :: Regex -> (String -> [String] -> String) -> String -> String

#### `runRegexFlags`

Unwrap `RegexFlags` type to the underlying record

    runRegexFlags :: RegexFlags -> { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean }

#### `search`

    search :: Regex -> String -> Number

#### `source`

    source :: Regex -> String

#### `split`

    split :: Regex -> String -> [String]

#### `sticky`

Flags where `sticky : true` and all others are false

    sticky :: RegexFlags

#### `test`

    test :: Regex -> String -> Boolean

#### `unicode`

Flags where `unicode : true` and all others are false

    unicode :: RegexFlags


## Module Data.String.Unsafe

### Values

#### `charAt`

    charAt :: Number -> String -> Char

#### `charCodeAt`

    charCodeAt :: Number -> String -> Number
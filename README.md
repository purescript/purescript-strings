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

#### `monoidRegexFlags`

    instance monoidRegexFlags :: Monoid RegexFlags

#### `regexFlagsBoolLike`

    instance regexFlagsBoolLike :: BoolLike RegexFlags

#### `regexFlagsSemiGroup`

    instance regexFlagsSemiGroup :: Semigroup RegexFlags

#### `showRegex`

    instance showRegex :: Show Regex


### Values

#### `flags`

    flags :: Regex -> RegexFlags

#### `match`

    match :: Regex -> String -> Maybe [String]

#### `newRegexFlags`

    newRegexFlags :: { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean } -> RegexFlags

#### `noFlags`

    noFlags :: RegexFlags

#### `onRegexFlags`

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

    runRegexFlags :: RegexFlags -> { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean }

#### `search`

    search :: Regex -> String -> Number

#### `source`

    source :: Regex -> String

#### `split`

    split :: Regex -> String -> [String]

#### `test`

    test :: Regex -> String -> Boolean


## Module Data.String.Unsafe

### Values

#### `charAt`

    charAt :: Number -> String -> Char

#### `charCodeAt`

    charCodeAt :: Number -> String -> Number
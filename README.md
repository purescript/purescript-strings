# Module Documentation

## Module Data.String

### Values

    charAt :: Number -> String -> String

    charCodeAt :: Number -> String -> Number

    drop :: Number -> String -> String

    fromCharCode :: Number -> String

    indexOf :: String -> String -> Number

    indexOf' :: String -> Number -> String -> Number

    joinWith :: String -> [String] -> String

    lastIndexOf :: String -> String -> Number

    lastIndexOf' :: String -> Number -> String -> Number

    length :: String -> Number

    localeCompare :: String -> String -> Number

    replace :: String -> String -> String -> String

    split :: String -> String -> [String]

    take :: Number -> String -> String

    toLower :: String -> String

    toUpper :: String -> String

    trim :: String -> String


## Module Data.String.Regex

### Types

    data Regex :: *


### Values

    match :: Regex -> String -> [String]

    regex :: String -> String -> Regex

    replace :: Regex -> String -> String -> String

    replace' :: Regex -> (String -> [String] -> String) -> String -> String

    search :: Regex -> String -> Number

    test :: Regex -> String -> Boolean
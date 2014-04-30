# Module Documentation

## Module Data.String

### Values

    charAt :: Prim.Number -> Prim.String -> Prim.String

    charCodeAt :: Prim.Number -> Prim.String -> Prim.Number

    drop :: Prim.Number -> Prim.String -> Prim.String

    fromCharCode :: Prim.Number -> Prim.String

    indexOf :: Prim.String -> Prim.String -> Prim.Number

    indexOf' :: Prim.String -> Prim.Number -> Prim.String -> Prim.Number

    joinWith :: Prim.String -> [Prim.String] -> Prim.String

    lastIndexOf :: Prim.String -> Prim.String -> Prim.Number

    lastIndexOf' :: Prim.String -> Prim.Number -> Prim.String -> Prim.Number

    length :: Prim.String -> Prim.Number

    localeCompare :: Prim.String -> Prim.String -> Prim.Number

    replace :: Prim.String -> Prim.String -> Prim.String -> Prim.String

    split :: Prim.String -> Prim.String -> [Prim.String]

    take :: Prim.Number -> Prim.String -> Prim.String

    toLower :: Prim.String -> Prim.String

    toUpper :: Prim.String -> Prim.String

    trim :: Prim.String -> Prim.String


## Module Data.String.Regex

### Types

    data Regex :: *


### Values

    match :: Regex -> Prim.String -> [Prim.String]

    regex :: Prim.String -> Prim.String -> Regex

    replaceR :: Regex -> Prim.String -> Prim.String -> Prim.String

    search :: Regex -> Prim.String -> Prim.Number

    test :: Regex -> Prim.String -> Prim.Boolean
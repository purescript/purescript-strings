# Module Documentation

## Module Data.Char


A type and functions for single characters.

#### `Char`

``` purescript
newtype Char
```

A unicode character.

#### `toString`

``` purescript
toString :: Char -> String
```

Returns the string of length `1` containing only the given character.

#### `toCharCode`

``` purescript
toCharCode :: Char -> Int
```

Returns the numeric Unicode value of the character.

#### `fromCharCode`

``` purescript
fromCharCode :: Int -> Char
```

Constructs a character from the given Unicode numeric value.

#### `eqChar`

``` purescript
instance eqChar :: Eq Char
```

Characters can be compared for equality with `==` and `/=`.

#### `ordChar`

``` purescript
instance ordChar :: Ord Char
```

Characters can be compared with `compare`, `>`, `>=`, `<` and `<=`.

#### `boundedChar`

``` purescript
instance boundedChar :: Bounded Char
```

Characters fall within the Unicode range.

#### `showChar`

``` purescript
instance showChar :: Show Char
```

Characters can be rendered as a string with `show`.




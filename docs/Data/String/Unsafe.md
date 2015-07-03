## Module Data.String.Unsafe

Unsafe string and character functions.

#### `charCodeAt`

``` purescript
charCodeAt :: Int -> String -> Int
```

Returns the numeric Unicode value of the character at the given index.

**Unsafe:** throws runtime exception if the index is out of bounds.

#### `charAt`

``` purescript
charAt :: Int -> String -> Char
```

Returns the character at the given index.

**Unsafe:** throws runtime exception if the index is out of bounds.

#### `char`

``` purescript
char :: String -> Char
```

Converts a string of length `1` to a character.

**Unsafe:** throws runtime exception if length is not `1`.



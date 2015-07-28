## Module Data.String

Wraps the functions of Javascript's `String` object.
A String represents a sequence of characters.
For details of the underlying implementation, see [String Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String).

#### `charAt`

``` purescript
charAt :: Int -> String -> Maybe Char
```

Returns the character at the given index, if the index is within bounds.

#### `fromChar`

``` purescript
fromChar :: Char -> String
```

Returns a string of length `1` containing the given character.

#### `singleton`

``` purescript
singleton :: Char -> String
```

Returns a string of length `1` containing the given character.
Same as `fromChar`.

#### `charCodeAt`

``` purescript
charCodeAt :: Int -> String -> Maybe Int
```

Returns the numeric Unicode value of the character at the given index,
if the index is within bounds.

#### `toChar`

``` purescript
toChar :: String -> Maybe Char
```

#### `null`

``` purescript
null :: String -> Boolean
```

Returns `true` if the given string is empty.

#### `uncons`

``` purescript
uncons :: String -> Maybe { head :: Char, tail :: String }
```

Returns the first character and the rest of the string,
if the string is not empty.

#### `takeWhile`

``` purescript
takeWhile :: (Char -> Boolean) -> String -> String
```

Returns the longest prefix (possibly empty) of characters that satisfy
the predicate:

#### `dropWhile`

``` purescript
dropWhile :: (Char -> Boolean) -> String -> String
```

Returns the suffix remaining after `takeWhile`.

#### `stripPrefix`

``` purescript
stripPrefix :: String -> String -> Maybe String
```

If the string starts with the given prefix, return the portion of the
string left after removing it, as a Just value. Otherwise, return Nothing.
* `stripPrefix "http:" "http://purescript.org" == Just "//purescript.org"`
* `stripPrefix "http:" "https://purescript.org" == Nothing`

#### `stripSuffix`

``` purescript
stripSuffix :: String -> String -> Maybe String
```

If the string ends with the given suffix, return the portion of the
string left after removing it, as a Just value. Otherwise, return Nothing.
* `stripSuffix ".exe" "psc.exe" == Just "psc"`
* `stripSuffix ".exe" "psc" == Nothing`

#### `fromCharArray`

``` purescript
fromCharArray :: Array Char -> String
```

Converts an array of characters into a string.

#### `contains`

``` purescript
contains :: String -> String -> Boolean
```

Checks whether the first string exists in the second string.

#### `indexOf`

``` purescript
indexOf :: String -> String -> Maybe Int
```

Returns the index of the first occurrence of the first string in the
second string. Returns `Nothing` if there is no match.

#### `indexOf'`

``` purescript
indexOf' :: String -> Int -> String -> Maybe Int
```

Returns the index of the first occurrence of the first string in the
second string, starting at the given index. Returns `Nothing` if there is
no match.

#### `lastIndexOf`

``` purescript
lastIndexOf :: String -> String -> Maybe Int
```

Returns the index of the last occurrence of the first string in the
second string. Returns `Nothing` if there is no match.

#### `lastIndexOf'`

``` purescript
lastIndexOf' :: String -> Int -> String -> Maybe Int
```

Returns the index of the last occurrence of the first string in the
second string, starting at the given index. Returns `Nothing` if there is
no match.

#### `length`

``` purescript
length :: String -> Int
```

Returns the number of characters the string is composed of.

#### `localeCompare`

``` purescript
localeCompare :: String -> String -> Ordering
```

Locale-aware sort order comparison.

#### `replace`

``` purescript
replace :: String -> String -> String -> String
```

Replaces the first occurence of the first argument with the second argument.

#### `take`

``` purescript
take :: Int -> String -> String
```

Returns the first `n` characters of the string.

#### `drop`

``` purescript
drop :: Int -> String -> String
```

Returns the string without the first `n` characters.

#### `count`

``` purescript
count :: (Char -> Boolean) -> String -> Int
```

Returns the number of contiguous characters at the beginning
of the string for which the predicate holds.

#### `split`

``` purescript
split :: String -> String -> Array String
```

Returns the substrings of the second string separated along occurences
of the first string.
* `split " " "hello world" == ["hello", "world"]`

#### `toCharArray`

``` purescript
toCharArray :: String -> Array Char
```

Converts the string into an array of characters.

#### `toLower`

``` purescript
toLower :: String -> String
```

Returns the argument converted to lowercase.

#### `toUpper`

``` purescript
toUpper :: String -> String
```

Returns the argument converted to uppercase.

#### `trim`

``` purescript
trim :: String -> String
```

Removes whitespace from the beginning and end of a string, including
[whitespace characters](http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
and [line terminators](http://www.ecma-international.org/ecma-262/5.1/#sec-7.3).

#### `joinWith`

``` purescript
joinWith :: String -> Array String -> String
```

Joins the strings in the array together, inserting the first argument
as separator between them.



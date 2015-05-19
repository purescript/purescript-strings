# Module Documentation

## Module Data.String.Regex


Wraps Javascript's `RegExp` object that enables matching strings with
patternes defined by regular expressions.
For details of the underlying implementation, see [RegExp Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp).

#### `Regex`

``` purescript
data Regex :: *
```

Wraps Javascript `RegExp` objects.

#### `showRegex`

``` purescript
instance showRegex :: Show Regex
```


#### `RegexFlags`

``` purescript
type RegexFlags = { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean }
```

Flags that control matching.

#### `noFlags`

``` purescript
noFlags :: RegexFlags
```

All flags set to false.

#### `regex`

``` purescript
regex :: String -> RegexFlags -> Regex
```

Constructs a `Regex` from a pattern string and flags.

#### `source`

``` purescript
source :: Regex -> String
```

Returns the pattern string used to construct the given `Regex`.

#### `flags`

``` purescript
flags :: Regex -> RegexFlags
```

Returns the `RegexFlags` used to construct the given `Regex`.

#### `renderFlags`

``` purescript
renderFlags :: RegexFlags -> String
```

Returns the string representation of the given `RegexFlags`.

#### `parseFlags`

``` purescript
parseFlags :: String -> RegexFlags
```

Parses the string representation of `RegexFlags`.

#### `test`

``` purescript
test :: Regex -> String -> Boolean
```

Returns `true` if the `Regex` matches the string.

#### `match`

``` purescript
match :: Regex -> String -> Maybe [Maybe String]
```

Matches the string against the `Regex` and returns an array of matches
if there were any. Each match has type `Maybe String`, where `Nothing`
represents an unmatched optional capturing group.
See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match).

#### `replace`

``` purescript
replace :: Regex -> String -> String -> String
```

Replaces occurences of the `Regex` with the first string. The replacement
string can include special replacement patterns escaped with `"$"`.
See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace).

#### `replace'`

``` purescript
replace' :: Regex -> (String -> [String] -> String) -> String -> String
```

Transforms occurences of the `Regex` using a function of the matched
substring and a list of submatch strings.
See the [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter).

#### `search`

``` purescript
search :: Regex -> String -> Int
```

Returns the index of the first match of the `Regex` in the string, or
`-1` if there is no match.

#### `split`

``` purescript
split :: Regex -> String -> [String]
```

Split the string into an array of substrings along occurences of the `Regex`.




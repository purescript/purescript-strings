-- | Wraps Javascript's `RegExp` object that enables matching strings with
-- | patternes defined by regular expressions.
-- | For details of the underlying implementation, see [RegExp Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp).
module Data.String.Regex
  ( Regex(..)
  , RegexFlags(..)
  , regex
  , source
  , flags
  , renderFlags
  , parseFlags
  , test
  , match
  , replace
  , replace'
  , search
  , split
  , noFlags
  ) where

import Prelude

import Data.Function (Fn4(), runFn4)
import Data.Maybe (Maybe(..))
import Data.Int ()
import Data.String (contains)

-- | Wraps Javascript `RegExp` objects.
foreign import data Regex :: *

foreign import showRegex' :: Regex -> String

instance showRegex :: Show Regex where
  show = showRegex'

-- | Flags that control matching.
type RegexFlags =
  { global :: Boolean
  , ignoreCase :: Boolean
  , multiline :: Boolean
  , sticky :: Boolean
  , unicode :: Boolean
  }

-- | All flags set to false.
noFlags :: RegexFlags
noFlags = { global     : false
          , ignoreCase : false
          , multiline  : false
          , sticky     : false
          , unicode    : false }

foreign import regex' :: String -> String -> Regex

-- | Constructs a `Regex` from a pattern string and flags.
regex :: String -> RegexFlags -> Regex
regex s f = regex' s $ renderFlags f

-- | Returns the pattern string used to construct the given `Regex`.
foreign import source :: Regex -> String

-- | Returns the `RegexFlags` used to construct the given `Regex`.
foreign import flags :: Regex -> RegexFlags

-- | Returns the string representation of the given `RegexFlags`.
renderFlags :: RegexFlags -> String
renderFlags f =
  (if f.global then "g" else "") ++
  (if f.ignoreCase then "i" else "") ++
  (if f.multiline then "m" else "") ++
  (if f.sticky then "y" else "") ++
  (if f.unicode then "u" else "")

-- | Parses the string representation of `RegexFlags`.
parseFlags :: String -> RegexFlags
parseFlags s =
  { global: contains "g" s
  , ignoreCase: contains "i" s
  , multiline: contains "m" s
  , sticky: contains "y" s
  , unicode: contains "u" s
  }

-- | Returns `true` if the `Regex` matches the string.
foreign import test :: Regex -> String -> Boolean

foreign import _match :: Fn4 Regex String (forall r. r -> Maybe r) (forall r. Maybe r) (Maybe (Array (Maybe String)))

-- | Matches the string against the `Regex` and returns an array of matches
-- | if there were any. Each match has type `Maybe String`, where `Nothing`
-- | represents an unmatched optional capturing group.
-- | See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match).
match :: Regex -> String -> Maybe (Array (Maybe String))
match r s = runFn4 _match r s Just Nothing

-- | Replaces occurences of the `Regex` with the first string. The replacement
-- | string can include special replacement patterns escaped with `"$"`.
-- | See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace).
foreign import replace :: Regex -> String -> String -> String

-- | Transforms occurences of the `Regex` using a function of the matched
-- | substring and a list of submatch strings.
-- | See the [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter).
foreign import replace' :: Regex -> (String -> Array String -> String) -> String -> String

-- | Returns the index of the first match of the `Regex` in the string, or
-- | `-1` if there is no match.
foreign import search :: Regex -> String -> Int

-- | Split the string into an array of substrings along occurences of the `Regex`.
foreign import split :: Regex -> String -> Array String

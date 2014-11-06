module Data.String.Regex (
  Regex(..),
  RegexFlags(..),
  regex,
  source,
  flags,
  renderFlags,
  parseFlags,
  test,
  match,
  replace,
  replace',
  search,
  split
  ) where

import Data.Function
import Data.Maybe
import Data.String (indexOf)

foreign import data Regex :: *

foreign import showRegex'
  """
  function showRegex$prime(r) {
    return '' + r;
  }
  """ :: Regex -> String

instance showRegex :: Show Regex where
  show = showRegex'

type RegexFlags =
  { global :: Boolean
  , ignoreCase :: Boolean
  , multiline :: Boolean
  , sticky :: Boolean
  , unicode :: Boolean
  }

foreign import regex'
  """
  function regex$prime(s1) {
    return function(s2) {
      return new RegExp(s1, s2);
    };
  }
  """ :: String -> String -> Regex

regex :: String -> RegexFlags -> Regex
regex source flags = regex' source $ renderFlags flags

foreign import source
  """
  function source(r) {
    return r.source;
  }
  """ :: Regex -> String

foreign import flags
  """
  function flags(r) {
    return {
      multiline: r.multiline,
      ignoreCase: r.ignoreCase,
      global: r.global,
      sticky: !!r.sticky,
      unicode: !!r.unicode
    };
  }
  """ :: Regex -> RegexFlags

renderFlags :: RegexFlags -> String
renderFlags flags =
  (if flags.global then "g" else "") ++
  (if flags.ignoreCase then "i" else "") ++
  (if flags.multiline then "m" else "") ++
  (if flags.sticky then "y" else "") ++
  (if flags.unicode then "u" else "")

parseFlags :: String -> RegexFlags
parseFlags s =
  { global: indexOf "g" s >= 0
  , ignoreCase: indexOf "i" s >= 0
  , multiline: indexOf "m" s >= 0
  , sticky: indexOf "y" s >= 0
  , unicode: indexOf "u" s >= 0
  }

foreign import test
  """
  function test(r) {
    return function(s) {
      return r.test(s);
    };
  }
  """ :: Regex -> String -> Boolean

foreign import _match
  """
  function _match(r, s, Just, Nothing) {
    var m = s.match(r);
    return m == null ? Nothing : Just(m);
  }
  """ :: forall r. Fn4 Regex String ([String] -> r) r r

match :: Regex -> String -> Maybe [String]
match r s = runFn4 _match r s Just Nothing

foreign import replace
  """
  function replace(r) {
    return function(s1) {
      return function(s2) {
        return s2.replace(r, s1);
      };
    };
  }
  """ :: Regex -> String -> String -> String

foreign import replace'
  """
  function replace$prime(r) {
    return function(f) {
      return function(s2) {
        return s2.replace(r, function(match) {
          return f(match)(Array.prototype.splice.call(arguments, 1, arguments.length - 3));
        });
      };
    };
  }
  """ :: Regex -> (String -> [String] -> String) -> String -> String

foreign import search
  """
  function search(r) {
    return function(s) {
      return s.search(r);
    };
  }
  """ :: Regex -> String -> Number

foreign import split
  """
  function split(r) {
    return function(s) {
      return s.split(r);
    };
  }
  """ :: Regex -> String -> [String]

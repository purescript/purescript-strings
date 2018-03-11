module Test.Data.String.NonEmpty (testNonEmptyString) where

import Data.String.NonEmpty

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array.NonEmpty as NEA
import Data.Array.Partial as AP
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..), fromJust, isNothing, maybe)
import Data.Semigroup.Foldable (class Foldable1, foldMap1Default)
import Partial.Unsafe (unsafePartial)
import Prelude (class Functor, Ordering(..), Unit, append, discard, negate, not, ($), (&&), (/=), (==))
import Test.Assert (ASSERT, assert)

testNonEmptyString :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testNonEmptyString = do
  log "fromString"
  assert $ fromString "" == Nothing
  assert $ fromString "hello" == Just (nes "hello")

  log "fromCharArray"
  assert $ fromCharArray [] == Nothing
  assert $ fromCharArray ['a', 'b'] == Just (nes "ab")

  log "fromNonEmptyCharArray"
  assert $ fromNonEmptyCharArray (NEA.singleton 'b') == singleton 'b'

  log "singleton"
  assert $ singleton 'a' == nes "a"

  log "cons"
  assert $ cons 'a' "bc" == nes "abc"
  assert $ cons 'a' "" == nes "a"

  log "snoc"
  assert $ snoc 'c' "ab" == nes "abc"
  assert $ snoc 'a' "" == nes "a"

  log "fromFoldable1"
  assert $ fromFoldable1 (NEA ['a']) == nes "a"
  assert $ fromFoldable1 (NEA ['a', 'b', 'c']) == nes "abc"

  log "charAt"
  assert $ charAt 0 (nes "a") == Just 'a'
  assert $ charAt 1 (nes "a") == Nothing
  assert $ charAt 0 (nes "ab") == Just 'a'
  assert $ charAt 1 (nes "ab") == Just 'b'
  assert $ charAt 2 (nes "ab") == Nothing
  assert $ charAt 2 (nes "Hello") == Just 'l'
  assert $ charAt 10 (nes "Hello") == Nothing

  log "charCodeAt"
  assert $ charCodeAt 0 (nes "a") == Just 97
  assert $ charCodeAt 1 (nes "a") == Nothing
  assert $ charCodeAt 0 (nes "ab") == Just 97
  assert $ charCodeAt 1 (nes "ab") == Just 98
  assert $ charCodeAt 2 (nes "ab") == Nothing
  assert $ charCodeAt 2 (nes "5 €") == Just 0x20AC
  assert $ charCodeAt 10 (nes "5 €") == Nothing

  log "toChar"
  assert $ toChar (nes "a") == Just 'a'
  assert $ toChar (nes "ab") == Nothing

  log "toCharArray"
  assert $ toCharArray (nes "a") == ['a']
  assert $ toCharArray (nes "ab") == ['a', 'b']
  assert $ toCharArray (nes "Hello☺\n") == ['H','e','l','l','o','☺','\n']

  log "toNonEmptyCharArray"
  assert $ toNonEmptyCharArray (nes "ab")
           == unsafePartial fromJust (NEA.fromArray ['a', 'b'])

  log "appendString"
  assert $ appendString (nes "Hello") " world" == nes "Hello world"
  assert $ appendString (nes "Hello") "" == nes "Hello"

  log "prependString"
  assert $ prependString "be" (nes "fore") == nes "before"
  assert $ prependString "" (nes "fore") == nes "fore"

  log "uncons"
  assert
    let m = uncons (nes "a")
    in m.head == 'a' && m.tail == Nothing
  assert $
    let m = uncons (nes "Hello World")
    in m.head == 'H' && m.tail == Just (nes "ello World")

  log "takeWhile"
  assert $ takeWhile (\c -> true) (nes "abc") == Just (nes "abc")
  assert $ takeWhile (\c -> false) (nes "abc") == Nothing
  assert $ takeWhile (\c -> c /= 'b') (nes "aabbcc") == Just (nes "aa")
  assert $ takeWhile (_ /= ':') (nes "http://purescript.org") == Just (nes "http")
  assert $ takeWhile (_ == 'a') (nes "xyz") == Nothing

  log "dropWhile"
  assert $ dropWhile (\c -> true) (nes "abc") == Nothing
  assert $ dropWhile (\c -> false) (nes "abc") == Just (nes "abc")
  assert $ dropWhile (\c -> c /= 'b') (nes "aabbcc") == Just (nes "bbcc")
  assert $ dropWhile (_ /= '.') (nes "Test.purs") == Just (nes ".purs")

  log "stripPrefix"
  assert $ stripPrefix (Pattern "") (nes "abc") == Just (nes "abc")
  assert $ stripPrefix (Pattern "a") (nes "abc") == Just (nes "bc")
  assert $ stripPrefix (Pattern "abc") (nes "abc") == Nothing
  assert $ stripPrefix (Pattern "!") (nes "abc") == Nothing
  assert $ stripPrefix (Pattern "http:") (nes "http://purescript.org") == Just (nes "//purescript.org")
  assert $ stripPrefix (Pattern "http:") (nes "https://purescript.org") == Nothing
  assert $ stripPrefix (Pattern "Hello!") (nes "Hello!") == Nothing

  log "stripSuffix"
  assert $ stripSuffix (Pattern ".exe") (nes "purs.exe") == Just (nes "purs")
  assert $ stripSuffix (Pattern ".exe") (nes "purs") == Nothing
  assert $ stripSuffix (Pattern "Hello!") (nes "Hello!") == Nothing

  log "contains"
  assert $ contains (Pattern "") (nes "abcd")
  assert $ contains (Pattern "bc") (nes "abcd")
  assert $ not (contains (Pattern "cb") (nes "abcd"))
  assert $ contains (Pattern "needle") (nes "haystack with needle") == true
  assert $ contains (Pattern "needle") (nes "haystack") == false

  log "indexOf"
  assert $ indexOf (Pattern "") (nes "abcd") == Just 0
  assert $ indexOf (Pattern "bc") (nes "abcd") == Just 1
  assert $ indexOf (Pattern "cb") (nes "abcd") == Nothing

  log "indexOf'"
  assert $ indexOf' (Pattern "") (-1) (nes "ab") == Nothing
  assert $ indexOf' (Pattern "") 0 (nes "ab") == Just 0
  assert $ indexOf' (Pattern "") 1 (nes "ab") == Just 1
  assert $ indexOf' (Pattern "") 2 (nes "ab") == Just 2
  assert $ indexOf' (Pattern "") 3 (nes "ab") == Nothing
  assert $ indexOf' (Pattern "bc") 0 (nes "abcd") == Just 1
  assert $ indexOf' (Pattern "bc") 1 (nes "abcd") == Just 1
  assert $ indexOf' (Pattern "bc") 2 (nes "abcd") == Nothing
  assert $ indexOf' (Pattern "cb") 0 (nes "abcd") == Nothing

  log "lastIndexOf"
  assert $ lastIndexOf (Pattern "") (nes "abcd") == Just 4
  assert $ lastIndexOf (Pattern "bc") (nes "abcd") == Just 1
  assert $ lastIndexOf (Pattern "cb") (nes "abcd") == Nothing

  log "lastIndexOf'"
  assert $ lastIndexOf' (Pattern "") (-1) (nes "ab") == Nothing
  assert $ lastIndexOf' (Pattern "") 0 (nes "ab") == Just 0
  assert $ lastIndexOf' (Pattern "") 1 (nes "ab") == Just 1
  assert $ lastIndexOf' (Pattern "") 2 (nes "ab") == Just 2
  assert $ lastIndexOf' (Pattern "") 3 (nes "ab") == Nothing
  assert $ lastIndexOf' (Pattern "bc") 0 (nes "abcd") == Nothing
  assert $ lastIndexOf' (Pattern "bc") 1 (nes "abcd") == Just 1
  assert $ lastIndexOf' (Pattern "bc") 2 (nes "abcd") == Just 1
  assert $ lastIndexOf' (Pattern "cb") 0 (nes "abcd") == Nothing

  log "length"
  assert $ length (nes "a") == 1
  assert $ length (nes "ab") == 2

  log "localeCompare"
  assert $ localeCompare (nes "a") (nes "a") == EQ
  assert $ localeCompare (nes "a") (nes "b") == LT
  assert $ localeCompare (nes "b") (nes "a") == GT

  log "replace"
  assert $ replace (Pattern "b") (NonEmptyReplacement (nes "!")) (nes "abc") == nes "a!c"
  assert $ replace (Pattern "b") (NonEmptyReplacement (nes "!")) (nes "abbc") == nes "a!bc"
  assert $ replace (Pattern "d") (NonEmptyReplacement (nes "!")) (nes "abc") == nes "abc"

  log "replaceAll"
  assert $ replaceAll (Pattern "[b]") (NonEmptyReplacement (nes "!")) (nes "a[b]c") == nes "a!c"
  assert $ replaceAll (Pattern "[b]") (NonEmptyReplacement (nes "!")) (nes "a[b]c[b]") == nes "a!c!"
  assert $ replaceAll (Pattern "x") (NonEmptyReplacement (nes "!")) (nes "abc") == nes "abc"

  log "take"
  assert $ take 0 (nes "ab") == Nothing
  assert $ take 1 (nes "ab") == Just (nes "a")
  assert $ take 2 (nes "ab") == Just (nes "ab")
  assert $ take 3 (nes "ab") == Just (nes "ab")
  assert $ take (-1) (nes "ab") == Nothing

  log "takeRight"
  assert $ takeRight 0 (nes "ab") == Nothing
  assert $ takeRight 1 (nes "ab") == Just (nes "b")
  assert $ takeRight 2 (nes "ab") == Just (nes "ab")
  assert $ takeRight 3 (nes "ab") == Just (nes "ab")
  assert $ takeRight (-1) (nes "ab") == Nothing

  log "drop"
  assert $ drop 0 (nes "ab") == Just (nes "ab")
  assert $ drop 1 (nes "ab") == Just (nes "b")
  assert $ drop 2 (nes "ab") == Nothing
  assert $ drop 3 (nes "ab") == Nothing
  assert $ drop (-1) (nes "ab") == Just (nes "ab")

  log "dropRight"
  assert $ dropRight 0 (nes "ab") == Just (nes "ab")
  assert $ dropRight 1 (nes "ab") == Just (nes "a")
  assert $ dropRight 2 (nes "ab") == Nothing
  assert $ dropRight 3 (nes "ab") == Nothing
  assert $ dropRight (-1) (nes "ab") == Just (nes "ab")

  log "count"
  assert $ count (_ == 'a') (nes "ab") == 1
  assert $ count (_ == 'a') (nes "aaab") == 3
  assert $ count (_ == 'a') (nes "abaa") == 1
  assert $ count (_ == 'c') (nes "abaa") == 0

  log "splitAt"
  let
    testSplitAt i str res =
      assert $ case splitAt i str of
        Nothing ->
          isNothing res
        Just { before, after } ->
          maybe false (\r ->
            r.before == before && r.after == after) res
  testSplitAt 0 (nes "a") (Just { before: Nothing, after: Just (nes "a") })
  testSplitAt 1 (nes "ab") (Just { before: Just (nes "a"), after: Just (nes "b") })
  testSplitAt 3 (nes "aabcc") (Just { before: Just (nes "aab"), after: Just (nes "cc") })
  testSplitAt (-1) (nes "abc") Nothing

  log "toLower"
  assert $ toLower (nes "bAtMaN") == nes "batman"

  log "toUpper"
  assert $ toUpper (nes "bAtMaN") == nes "BATMAN"

  log "trim"
  assert $ trim (nes "  abc  ") == Just (nes "abc")
  assert $ trim (nes "   \n") == Nothing

  log "joinWith"
  assert $ joinWith "" [] == ""
  assert $ joinWith "" [nes "a", nes "b"] == "ab"
  assert $ joinWith "--" [nes "a", nes "b", nes "c"] == "a--b--c"

  log "join1With"
  assert $ join1With "" (NEA [nes "a", nes "b"]) == nes "ab"
  assert $ join1With "--" (NEA [nes "a", nes "b", nes "c"]) == nes "a--b--c"
  assert $ join1With ", " (NEA [nes "apple", nes "banana"]) == nes "apple, banana"
  assert $ join1With "" (NEA [nes "apple", nes "banana"]) == nes "applebanana"

  log "joinWith1"
  assert $ joinWith1 (nes " ") (NEA ["a", "b"]) == nes "a b"
  assert $ joinWith1 (nes "--") (NEA ["a", "b", "c"]) == nes "a--b--c"
  assert $ joinWith1 (nes ", ") (NEA ["apple", "banana"]) == nes "apple, banana"
  assert $ joinWith1 (nes "/") (NEA ["a", "b", "", "c", ""]) == nes "a/b//c/"

nes :: String -> NonEmptyString
nes = unsafePartial unsafeFromString

newtype NEA a = NEA (Array a)

derive newtype instance functorNEA :: Functor NEA
derive newtype instance foldableNEA :: Foldable NEA

instance foldable1NEA :: Foldable1 NEA where
  foldMap1 a = foldMap1Default a
  fold1 (NEA as) = foldl append (unsafePartial AP.head as) (unsafePartial AP.tail as)

{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "strings"
, dependencies = [ "arrays"
          , "control"
          , "either"
          , "enums"
          , "foldable-traversable"
          , "gen"
          , "integers"
          , "maybe"
          , "newtype"
          , "nonempty"
          , "partial"
          , "prelude"
          , "tailrec"
          , "tuples"
          , "unfoldable"
          , "unsafe-coerce"
          ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}

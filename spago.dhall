{ name = "test-unit"
, dependencies =
  [ "aff"
  , "either"
  , "prelude"
  , "effect"
  , "quickcheck"
  , "free"
  , "strings"
  , "lists"
  , "avar"
  , "console"
  , "control"
  , "datetime"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lcg"
  , "maybe"
  , "newtype"
  , "parallel"
  , "refs"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

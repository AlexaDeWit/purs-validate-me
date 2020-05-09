{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purs-validate-me"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "bifunctors"
  , "either"
  , "maybe"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

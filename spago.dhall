{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "console"
  , "debug"
  , "deku"
  , "deku-toplevel"
  , "effect"
  , "either"
  , "event"
  , "filterable"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
}

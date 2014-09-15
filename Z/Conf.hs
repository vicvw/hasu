module Conf
    ( conf
    ) where


import Z


conf :: [ZExpr]
conf =
    [ path    [ "~/コ/bin"
              , "~/.cabal/bin"
              , "~/.gem/ruby/2.1.0/bin"
              ]
    , export  "PATH"

    , var     "LANG"    "ja_JP.UTF-8"         []
    , export  "LANG"
    ]

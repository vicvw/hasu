module Dispatch
    ( run
    ) where


import Control.Monad          (void)

import Data.List              (find)
import Data.Maybe             (fromJust)

import System.Cmd             (system)
import System.FilePath.Posix  (takeExtension)

import Text.Printf            (printf)
import Text.Regex.PCRE        ((=~))


run :: FilePath -> IO ()
run f = dispatch f f


dispatch :: FilePath -> Handler
dispatch f
    = fromJust
    $ lookup'
        (takeExtension f)
        extensionMap

    where
    lookup' ext
        = fmap snd
        . find (any (ext =~) . fst)


extensionMap =
    [ ["^$"]    --> cat
    , ["hs"]    --> haskell
    , ["rb"]    --> ruby
    , ["scala"] --> scala
    , ["."]     --> unknown
    ]


cat     = generic "cat"
haskell = generic "runhaskell"
ruby    = generic "ruby"
scala   = undefined
unknown = const $ putStrLn "unknown."


generic :: String -> Handler
generic cmd f = do
    clear
    void . system $ printf "%s %s" cmd f


clear = system "clear"


(-->) = (,)


type Handler = FilePath -> IO ()

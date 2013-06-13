module Dispatch
    ( run
    , watch
    ) where


import Control.Monad          (forever, unless, void)
import Control.Concurrent     (threadDelay)

import Data.List              (find)
import Data.Maybe             (fromJust)

import System.Cmd             (system)
import System.FilePath.Posix  (takeExtension)
import System.INotify         (addWatch, withINotify, Event (Modified), EventVariety (Modify))

import Text.Printf            (printf)
import Text.Regex.PCRE        ((=~))


extensionMap :: [([String], Handler)]
extensionMap =
    [ ["sh", "run"]  --> bash
    , ["c"]          --> c
    , ["^$", "txt"]  --> cat
    , ["c++", "cpp"] --> cpp
    , ["s"]          --> dlx
    , ["hs"]         --> haskell
    , ["java"]       --> java
    , ["py"]         --> python
    , ["rb"]         --> ruby
    , ["scala"]      --> scala
    , ["."]          --> unknown
    ]


bash, c, cat, cpp, dlx, haskell, java, python, ruby, scala :: Handler

bash    = generic "bash"
cat     = generic "cat"
dlx     = generic "dlx"
haskell = generic "runhaskell"
python  = generic "python"
ruby    = generic "ruby"

c       = unknown
cpp     = unknown
java    = unknown
scala   = unknown


watch :: FilePath -> [FilePath] -> Maybe Command -> Handler
watch dir igns cmd args file = withINotify $ \inotify -> do
    addWatch inotify [Modify] dir $ handle igns cmd args file
    wait

    where
    wait = void . forever . threadDelay $ 10^6


handle :: [FilePath] -> Maybe Command -> Arguments -> FilePath -> Event -> IO ()
handle igns cmd args file (Modified _ path) =
    unless (isIgnored $ fromJust path) $
        run cmd args file

    where
    isIgnored f = any (f =~) ignored
    ignored = ["\\.swp$", "\\.o$", "\\.hi$"] ++ igns


run :: Maybe Command -> Handler
run mcmd args file =
    maybe (dispatch file args file)
          (\cmd -> custom cmd args file)
          mcmd


dispatch :: FilePath -> Handler
dispatch file
    = fromJust
    $ lookup'
        (takeExtension file)
        extensionMap

    where
    lookup' ext
        = fmap snd
        . find (any (ext =~) . fst)


custom :: Command -> Handler
custom cmd args file = do
    void . system $ printf "%s %s %s" cmd file (unwords args)
    putStrLn "\n完"


generic :: Command -> Handler
generic cmd args file = do
    void . system $ "clear"
    custom cmd args file


unknown :: Handler
unknown _ file = putStrLn $ printf "cannot handle: %s" file


(-->) :: a -> b -> (a, b)
(-->) = (,)


type Arguments = [String]
type Command   = String
type Handler   = Arguments -> FilePath -> IO ()

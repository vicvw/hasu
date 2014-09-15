module Handlers
    ( handlers
    , custom
    ) where


import General


import Control.Monad          (void)
import Text.Printf            (printf)

import System.Exit            (ExitCode (..))
import System.FilePath
import System.Process         (system)


handlers :: [([String], Handler)]
handlers =
    [ ["sh", "run"]  --> bash
    , ["c"]          --> c
    , ["^$", "txt"]  --> cat
    , ["c++", "cpp"] --> cpp
    , ["hs"]         --> haskell
    , ["java"]       --> java
    , ["py"]         --> python
    , ["rb"]         --> ruby
    , ["scala"]      --> scala
    , ["."]          --> unknown
    ]


bash, c, cat, cpp, haskell, java, python, ruby, scala, unknown :: Handler

bash    = generic "bash"
cat     = generic "cat"
haskell = generic "runhaskell"
python  = generic "python"
ruby    = generic "ruby"

java args file = do
    clear

    putStrLn "中"
    code <- system $ printf "javac %s" file

    case code of
        ExitSuccess   -> do
            clear
            void . system $ printf "java %s %s" (takeBaseName file) (unwords args)
        ExitFailure _ -> return ()

c     = unknown
cpp   = unknown
scala = unknown

unknown _ = printf "cannot handle: %s\n"


generic, custom :: Command -> Handler
generic cmd args file = do
    clear
    custom (printf "%s %%s %%s" cmd) args file


custom cmd args file = do
    void . system $ printf cmd file (unwords args)
    putStrLn "\n完"


clear :: IO ()
clear = void $ system "clear"

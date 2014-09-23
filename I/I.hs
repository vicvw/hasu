module Main (main) where


import Data.Functor       (void)

import System.Environment (getArgs)
import System.Exit        (ExitCode (..))
import System.Process     (system)


main :: IO ()
main = do
    [cond, true, false, const] <- getArgs
    code <- system $ zsh cond

    void . system . zsh . prepend const
        $ case code of
          ExitSuccess   -> true
          ExitFailure _ -> false

    where
    prepend = (unwords .) . (. return) . (:)


zsh :: String -> String
zsh s = concat ["zsh -ic \"", s, "\""]

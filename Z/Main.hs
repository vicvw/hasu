module Main (main) where


import Z
import qualified Conf

import Control.Monad  (unless)


main :: IO ()
main = if isValid conf
    then mapM_ print conf
    else do
        unless (depsExist conf) $
            mapM_ print $ missingDeps conf

        unless (validOrder conf) $ do
            print . fst $ invalidOrder conf
            putStrLn "|"
            mapM_ print . snd $ invalidOrder conf

    where
    conf = Conf.conf
    -- conf =
    --     [ var "HOME" "$HOME:/etc/hell"
    --           []
    --     , alias "s" "sudo"
    --           ["HOME"]
    --     , alias "pm" "s pacman"
    --           ["s", "HOME"]
    --     , aliasS "xz" "pm -U"
    --           ["pm"]
    --     ]

module Main (main) where


import Omo


import Control.Monad  (void)
import Text.Printf    (printf)
import System.Cmd     (system)


main :: IO ()
main = void . system =<< test

    where
    test = different ""
        $ 風 "echo blub"
        . 空 ""

    spawner :: Double -> String -> String
    spawner = printf "sh -c 'sleep %f; exec %s'"

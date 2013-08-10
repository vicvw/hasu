module Main (main) where


import Omo


import Control.Monad      (void)
import System.Cmd         (system)


main :: IO ()
main = void . system =<< test

    where
    test = different ""
        $ 風 "echo blub"
        . 空 ""

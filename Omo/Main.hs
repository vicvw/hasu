module Main (main) where


import Omo
import Omo.IO


main :: IO ()
main = differentIO
    $ 風 (putStrLn "kaze")
    . 空 (putStrLn "sora")

module Main (main) where


import Canvas
import Shapes.Point


main :: IO ()
main = putStrLn $ render test
-- main = putStrLn . render $ empty (1000, 1000) (h '.')

    where
    test = flip draw (empty (25, 25) $ h '.')
        [ point ( 0,  0) $ d '█'
        , point ( 0, 24) $ d '█'
        , point (24,  0) $ d '█'
        , point (24, 24) $ d '█'
        ]

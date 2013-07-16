module Main (main) where


import Canvas
import Objects.Point
import Objects.Line


main :: IO ()
main = putStrLn $ render test
-- main = putStrLn . render $ empty (1000, 1000) (h '.')

    where
    test = draw
             [ line  ( 1,  1) ( 22, 23) $ d '#'
             ]
         . draw
             [ point ( 0,  0) $ h '1'
             , point ( 0, 24) $ h '2'
             , point (24,  0) $ h '3'
             , point (24, 24) $ h '4'
             ]
         $ empty (25, 25) $ h '·'

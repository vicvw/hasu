module Main where


import Hosuto

-- import System.Environment (getArgs)


main :: IO ()
main = do
    -- args <- getArgs

    spawnIO test

    where
    test = actionPack
        -- { 風 = Just . action 0 $ print "kaze"
        { 風 = Just . actionIO 0 $ print "kaze"
        }

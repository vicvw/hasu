module Main (main) where


import Conf (conf)


main :: IO ()
main = putStr . unlines $ map show conf

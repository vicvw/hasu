module Main (main) where


import Z
import qualified Conf

import Control.Monad  (unless)


main :: IO ()
main = do
    putStr . unlines $ map show conf

    where
    conf = Conf.conf

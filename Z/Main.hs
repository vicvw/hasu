module Main (main) where


import Z
import qualified Conf

import Control.Monad  (unless)


main :: IO ()
main = do
    mapM_ (putStrLn . show) conf

    where
    conf = Conf.conf

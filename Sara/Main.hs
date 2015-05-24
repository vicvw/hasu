module Main (main) where


import Omo


import Control.Applicative  ((<$>))
import Data.List            ((\\))
import System.Environment   (getArgs)
import System.Exit          (ExitCode (..))
import System.Process       (readProcessWithExitCode)


main :: IO ()
main = do
    (e,o,_) <- readProcessWithExitCode "packer" ["--quickcheck"] ""
    sin:_   <- getArgs
    kuro    <- kurod

    putStrLn $ case e of
        ExitFailure _ -> sin
        ExitSuccess
           -> ji sin
            . length
            . (\\ kuro)
            $ lines o

    where
    kurod = different []
        $ 風 ["fcitx-mozc-ut", "xcape-git"]
        . 空 ["compton-git", "packer", "xcape-git"]


ji :: String -> Int -> String
ji sin 0 = sin
ji _   n
    | n < 10    = iti !! (n - 1)
    | n < 100   = "十"
    | n < 1000  = "百"
    | otherwise = "千"

    where
    iti = return <$> "一二三四五六七八九"

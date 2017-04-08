module Main (main) where


import Nara
import Omo


import Data.List            ((\\))
import System.Environment   (getArgs)
import System.Exit          (ExitCode (..))
import System.Process       (readProcessWithExitCode)


main :: IO ()
main = do
    (e,o,_) <- readProcessWithExitCode "packer" ["--quickcheck"] ""
    -- (_,p,_) <- readProcessWithExitCode "pacman" ["-Quq"] ""
    sin:_   <- getArgs
    kuro    <- kurod

    let success = ji sin . length . (\\ kuro) . lines -- . (p ++)

    putStrLn . if_ o (/= "\n")
        success
        $ case e of
            ExitFailure c -> fi (c == 1) (const $ success "") (const "失")
            ExitSuccess   -> success

    where
    kurod = different []
        $ 風 ["anki", "qtwebkit", "xcape-git"]
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

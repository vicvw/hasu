module Main (main) where


import Control.Applicative  ((<$>))
import System.Environment   (getArgs)
import System.Process       (readProcessWithExitCode)


main :: IO ()
main = do
    utd:_     <- getArgs
    (_, o, _) <- readProcessWithExitCode "packer" ["--quickcheck"] ""

    putStrLn . ji utd . length $ lines o


ji :: String -> Int -> String
ji utd 0 = utd
ji _   n
    | n < 10    = iti !! (n - 1)
    | n < 100   = "十"
    | n < 1000  = "百"
    | otherwise = "千"

    where
    iti = return <$> "一二三四五六七八九"

{-# LANGUAGE LambdaCase #-}

module Main (main) where


import System.Environment (getArgs)
import System.Process     (readProcessWithExitCode)


main :: IO ()
main = do
    getArgs >>= \case
        -- ["t"] -> do
        _ -> do
            (_,o,_) <- readProcessWithExitCode "df" ["-h"] ""
            putStrLn . (!! 2) . words . (!! 6) $ lines o

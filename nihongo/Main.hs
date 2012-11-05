module Main where

import Syllabary

import Data.Char (toLower)
import Control.Monad.Identity



main = putStrLn . runIdentity $ transcribe hiragana "kyuubi"
-- main = case transcribe hiragana "baka! lulu" of
--     Just trans -> putStrLn trans
--     Nothing    -> putStrLn "failed."


        
transcribe :: Syllabary -> String -> Identity String
transcribe syllabary = transcribe' 3 . map toLower
  where
    transcribe' _ ""     = return ""
    transcribe' n string = case partial n of
        Just s  -> fmap (s ++) $ transcribe' 3 (drop n string)
        Nothing -> if n > 1
                   then transcribe' (n - 1) string
                   else fmap ((take n string) ++) $ transcribe' 3 (drop n string)
      where
        partial n = lookup (take n string) syllabary

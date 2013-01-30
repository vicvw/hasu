module Main where

import Syllabary

import Data.Char (toLower)
import Control.Monad.Identity
import System.Environment



main = putStrLn
     . runIdentity
     . transcribe katakana
     . head
   =<< getArgs


        
transcribe :: Syllabary -> String -> Identity String
transcribe syllabary = transcribe' 3 . map toLower
  where
    transcribe' _ ""     = return ""
    transcribe' n string = case partial n of
        Just s  -> liftM (s ++) $ transcribe' 3 (drop n string)
        Nothing -> if n > 1
                   then transcribe' (n - 1) string
                   else liftM ((take n string) ++) $ transcribe' 3 (drop n string)
                   -- else transcribe' 3 (drop n string)
      where
        partial n = lookup (take n string) syllabary

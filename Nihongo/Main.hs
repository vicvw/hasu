module Main (main) where


import Syllabary

import Data.Char          (toLower)
import System.Environment (getArgs)


main :: IO ()
main = putStrLn
     . transcribe katakana
     . head
   =<< getArgs


transcribe :: Syllabary -> String -> String
transcribe syllabary = transcribe' 3 . map toLower
    where
    transcribe' _ ""     = ""
    transcribe' n string = maybe
        ( if n > 1
          then transcribe' (n - 1) string
          else h ++ transcribe' 3 t )
        (++ transcribe' 3 t) $
        partial n

        where
        (h, t)  = splitAt n string
        partial = (`lookup` syllabary) . (`take` string)

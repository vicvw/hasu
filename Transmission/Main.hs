module Main (main) where


import Control.Applicative  ((<$>), (<*>), (<*), (*>))

import Data.List            (delete, genericLength)
import Data.Maybe           (catMaybes)

import System.Environment   (getArgs)
import System.Process       (readProcessWithExitCode)

import Text.Parsec
import Text.Parsec.String


main :: IO ()
main = do
    what:_  <- getArgs
    (_,o,_) <- readProcessWithExitCode "transmission-remote" ["-l"] ""

    let parsed = parse progresses "" o

    print
        . either' parsed (const 0)
        . (. filter (< 100) . catMaybes)
        . if' null (const 0)
        $ if what == "a"
          then ceiling . average . (delete =<< maximum)
          else maximum

    where
    progresses = firstLine *> many progress
    firstLine  = manyTill anyChar newline
    progress = fmap (read <$>)
        $ spaces
       *> many1 digit
       *> optional (char '*')
       *> spaces
       *> optionMaybe (many1 digit)
       <* manyTill anyChar newline


average :: (Real a, Fractional b) => [a] -> b
average = if' null (const 0) $
    (/) . realToFrac . sum <*> genericLength

-- average [] = 0
-- average xs = realToFrac (sum xs) / genericLength xs


either' :: Either a b -> (a -> c) -> (b -> c) -> c
either' = flip $ flip . either


if' :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
if' test t f x = ($ x) $ if test x then t else f

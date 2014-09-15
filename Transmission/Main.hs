module Main (main) where


import Control.Applicative  ((<$>), (<*), (*>))
import Data.List            (genericLength)
import Data.Maybe           (catMaybes)
import Text.ParserCombinators.Parsec
import System.Environment   (getArgs)
import System.Process       (readProcessWithExitCode)


main :: IO ()
main = do
    what:_      <- getArgs
    (_, out, _) <- readProcessWithExitCode "transmission-remote" ["-l"] ""

    let parsed = parse progresses "" out

    print
        . either' parsed (const 0)
        . (. filter (< 100) . catMaybes)
        . if' null (const 0)
        $ if what == "a"
          then ceiling . average
          else maximum

    where
    progresses :: Parser [Maybe Int]
    progresses = map (read <$>) <$> (firstLine *> many progress)
    firstLine  = manyTill anyChar newline
    progress
        = spaces
       *> many1 digit
       *> spaces
       *> optionMaybe (many1 digit)
       <* manyTill anyChar newline


average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs


either' :: Either a b -> (a -> c) -> (b -> c) -> c
either' = flip $ flip . either


if' :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
if' test t f x = ($ x) $ if test x then t else f

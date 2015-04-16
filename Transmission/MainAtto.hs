module Main (main) where


import Control.Applicative  ((<$>), (<*>), (<*), (*>))

import Data.Attoparsec.Text
import Data.List            (delete, genericLength)
import Data.Maybe           (catMaybes)
import Data.Text            (pack)

import System.Environment   (getArgs)
import System.Process       (readProcessWithExitCode)


main :: IO ()
main = do
    what:_    <- getArgs
    (_, o, _) <- readProcessWithExitCode "transmission-remote" ["-l"] ""

    let parsed = maybeResult . parse progresses $ pack o

    print
        . maybe' parsed 0
        . (. filter (< 100) . catMaybes)
        . if' null (const 0)
        $ if what == "a"
          then ceiling . average . (delete =<< maximum)
          else maximum

    where
    progresses = firstLine *> progress `sepBy` endOfLine
    firstLine  = manyTill anyChar endOfLine
    progress
        = option Nothing
        . (Just . read <$>)
        $ skipSpace
       *> many1 digit
       *> skipMany (char '*')
       *> skipSpace
       *> many1 digit
       <* skipWhile (not . isEndOfLine)


average :: (Real a, Fractional b) => [a] -> b
average = if' null (const 0) $
    (/) . realToFrac . sum <*> genericLength


maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' = flip $ flip . maybe


if' :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
if' test t f x = ($ x) $ if test x then t else f

module Misc
    ( screenWidth
    , screenHeight
    , screenSize
    ) where

import Text.Printf    (printf)
import Text.ParserCombinators.Parsec
import System.Process (readProcess)


data Screen = Screen
    { screenWidth   :: Integer
    , screenHeight  :: Integer }


screenSize :: Integer -> IO Screen
screenSize n = do
    x <- xrandr
    case parse (parse_screenSize n) "xrandr" x of
        Right d -> return d
        Left _  -> error "xrandr pooped."


parse_screenSize :: Integer -> GenParser Char a Screen
parse_screenSize screen = do
    manyTill anyChar . try $ string (printf "Screen %i" screen)
    manyTill anyChar . try $ string "current "
    x <- manyTill digit $ string " x "
    y <- manyTill digit $ char ','
    return $ Screen (read x) (read y)


xrandr :: IO String
xrandr = readProcess "xrandr" [] []

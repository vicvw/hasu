module Dzen.Misc
    ( ScreenSize (..)
    , screenSize
    ) where


import Text.Printf    (printf)
import Text.ParserCombinators.Parsec
import System.Process (readProcess)


data ScreenSize = ScreenSize
    { screenWidth   :: Integer
    , screenHeight  :: Integer }


screenSize :: Integer -> IO ScreenSize
screenSize n = do
    x <- xrandr
    case parse (parseScreenSize n) "xrandr" x of
        Right d -> return d
        Left _  -> error "xrandr pooped."


parseScreenSize :: Integer -> GenParser Char a ScreenSize
parseScreenSize screen = do
    manyTill anyChar . try $ string (printf "Screen %i" screen)
    manyTill anyChar . try $ string "current "
    x <- manyTill digit $ string " x "
    y <- manyTill digit $ char ','
    return $ ScreenSize (read x) (read y)


xrandr :: IO String
xrandr = readProcess "xrandr" [] ""

module Volume
    ( getVolume
    , getLinearVolume
    , isMuted
    , toggleMute
    , decreaseVolume
    , increaseVolume
    , linearize
    , Level
    ) where


import Control.Monad    (void)
import Data.Either      (either)
import Data.Maybe       (fromJust)
import System.Process   (readProcess)
import Text.ParserCombinators.Parsec


getVolume :: IO Level
getVolume = do
    sinks <- pacmd "list-sinks"

    either fail
           succeed
         $ parse parseVolume "volume" sinks

    where
    parseVolume = do
        manyTill anyChar $ char '*'
        manyTill anyChar . try $ string "volume: 0:"
        skipMany1 space
        manyTill digit $ string "%"

    fail    = const $ error "poop."
    succeed = return . read


getLinearVolume :: IO Level
getLinearVolume = fmap linearize getVolume


isMuted :: IO Bool
isMuted = do
    sinks <- pacmd "list-sinks"

    either fail
           succeed
         $ parse parseMuted "muted" sinks

    where
    parseMuted = do
        manyTill anyChar $ char '*'
        manyTill anyChar . try $ string "muted:"
        skipMany1 space
        manyTill letter newline

    fail    = const $ error "poop."
    succeed = return . (== "yes")


toggleMute :: IO ()
toggleMute = void $ amixer "sset Master toggle"


decreaseVolume, increaseVolume :: IO ()
decreaseVolume = void $ amixer "-c 0 set Master 4-"
increaseVolume = void $ amixer "-c 0 set Master 4+"


amixer :: String -> IO String
amixer args = readProcess "amixer" (words args) ""


pacmd :: String -> IO String
pacmd args = readProcess "pacmd" (words args) ""


linearize :: Level -> Level
linearize = fromJust . (`lookup` mapping)

    where
    steps = [ 0, 18, 20, 22, 25, 28, 32, 35, 40
            , 45, 50, 56, 63, 71, 79, 89, 100 ]

    mapping = steps
        `zip` map (* (100 / fromIntegral (length steps - 1)))
                  [0..]
    -- mapping = map (id &&& \x -> x * (1920 `div` 4 `div` 16 - 1) * 100 `div` (1920 `div` 4) + (100 `div` length steps)) steps


getMatches :: (String, String, String, [String]) -> [String]
getMatches regex =
    let (_, _, _, matches) = regex :: (String, String, String, [String])
    in  matches


getFirstMatch :: (String, String, String, [String]) -> String
getFirstMatch regex = head $ getMatches regex


type Level = Double

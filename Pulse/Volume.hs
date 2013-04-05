module Pulse.Volume
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
import Data.Maybe       (fromJust)
import Text.Regex.PCRE  ((=~))
import System.Process   (readProcess)


getVolume :: IO Level
getVolume = do
    amixer <- amixer "sget Master"

    let level = getFirstMatch $ amixer =~ "\\[(\\d+)%\\]"

    return $ read level


getLinearVolume :: IO Level
getLinearVolume = fmap linearize getVolume


isMuted :: IO Bool
isMuted = do
    amixer <- amixer "sget Master"

    let muted = getFirstMatch $ amixer =~ "\\[(\\w*)\\]"

    return $ muted == "off"


toggleMute :: IO ()
toggleMute = void $ amixer "sset Master toggle"


decreaseVolume, increaseVolume :: IO ()
decreaseVolume = void $ amixer "-c 0 set Master 4- toggle"
increaseVolume = void $ amixer "-c 0 set Master 4+ toggle"


amixer :: String -> IO String
amixer args = readProcess "amixer" (words args) ""


linearize :: Level -> Level
linearize = fromJust . (`lookup` mapping)

    where
    steps = [ 0, 18, 20, 22, 25, 28, 32, 35, 40
            , 45, 50, 56, 63, 71, 79, 89, 100 ]

    mapping = steps
        `zip` map (* (100 / fromIntegral (length steps - 1)))
                  [0..]
    -- mapping = map (id &&& \x -> x * (1920 `div` 4 `div` 16 - 1) * 100 `div` (1920 `div` 4) + (100 `div` length steps)) steps


getFirstMatch :: (String, String, String, [String]) -> String
getFirstMatch regex =
    let (_, _, _, first:_) = regex :: (String, String, String, [String])
    in  first


type Level = Double

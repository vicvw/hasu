module Volume
    ( volumeOut
    , volumeOutLinear
    , isMutedOut
    , isMutedApp
    , toggleMuteOut
    , toggleMuteApp
    , muteOut
    , muteApp
    , unmuteOut
    , unmuteApp
    , setVolumeOut
    , decreaseVolumeOut
    , increaseVolumeOut
    , Level
    ) where


import Omo            (different, 風, 空)


import Control.Applicative hiding ((<|>), many)
import Control.Monad  (void, unless, when)

import Data.List      (minimumBy)
import Data.Maybe     (fromJust, fromMaybe)
import Data.Ord       (comparing)

import System.Process (readProcess)

import Text.ParserCombinators.Parsec


volumeOut :: IO Level
volumeOut = do
    current <- indexCurrentOut

    parsePacmd ["list-sinks"] read
        $ manyTill anyChar (string $ "* index: " ++ current)
       *> manyTill anyChar (try $ string "volume: 0:")
       *> skipMany1 space
       *> many1 digit


volumeOutLinear :: IO Level
volumeOutLinear = hostVolumeSteps >>=
    (`fmap` volumeOut) . linearize

    where
    linearize steps = fromJust . (`lookup` mapping steps)

    mapping steps = zip steps $
        map (* (100 / fromIntegral (length steps - 1)))
            [0..]


isMutedOut :: IO Bool
isMutedOut = do
    current <- indexCurrentOut

    parsePacmd ["list-sinks"] (== "yes")
        $ manyTill anyChar (string $ "* index: " ++ current)
       *> manyTill anyChar (try $ string "muted: ")
       *> yesNo


isMutedApp :: String -> IO Bool
isMutedApp app = do
    index <- indexApp app

    parsePacmd ["list-sink-inputs"] (== "yes")
        $ manyTill anyChar (try . string $ "index: " ++ index)
       *> manyTill anyChar (try $ string "muted: ")
       *> yesNo


yesNo :: Parser String
yesNo = string "yes" <|> string "no"


tillBinary :: String -> Parser String
tillBinary app = string "application.process.binary = \""
    *> manyTill anyChar (string app)


muteOut, unmuteOut :: IO ()
muteOut   = changeMuteOut unless
unmuteOut = changeMuteOut when


changeMuteOut :: (Bool -> IO () -> IO a) -> IO a
changeMuteOut cond = isMutedOut >>=
    (`cond` toggleMuteOut)


toggleMuteOut :: IO ()
toggleMuteOut = toggleMute
    indexCurrentOut
    isMutedOut
    "set-sink-mute"


muteApp, unmuteApp :: String -> IO ()
muteApp   = changeMuteApp unless
unmuteApp = changeMuteApp when


changeMuteApp :: (Bool -> IO () -> IO a) -> String -> IO a
changeMuteApp cond app = isMutedApp app >>=
    (`cond` toggleMuteApp app)


toggleMuteApp :: String -> IO ()
toggleMuteApp app = toggleMute
    (indexApp app)
    (isMutedApp app)
    "set-sink-input-mute"


toggleMute :: IO String -> IO Bool -> String -> IO ()
toggleMute indexF mutedF pacmdArg = do
    index <- indexF
    muted <- mutedF

    execPacmd . ([pacmdArg, index] ++) $
        if muted
        then ["no"]
        else ["yes"]


decreaseVolumeOut, increaseVolumeOut :: IO ()
decreaseVolumeOut = changeVolumeOut fst
increaseVolumeOut = changeVolumeOut snd


changeVolumeOut :: ((Maybe Level, Maybe Level) -> Maybe Level) -> IO ()
changeVolumeOut selector = do
    volume <- volumeOut
    vmap   <- volumeMap

    let newv = fromMaybe volume
             . selector
             . fromJust
             . (`lookup` vmap)
             $ closestOn fst volume vmap

    setVolumeOut newv

    where
    closestOn f a = f . minimumBy (comparing $ abs . subtract a . f)


setVolumeOut :: Level -> IO ()
setVolumeOut level = do
    current <- indexCurrentOut
    vsteps  <- subtract 1 `fmap` volumeSteps

    let newv = round $ level / 100 * fromIntegral vsteps

    execPacmd ["set-sink-volume", current, show newv]


volumeMap :: IO [(Level, (Maybe Level, Maybe Level))]
volumeMap = do
    steps <- hostVolumeSteps

    let steps' = map return steps

    return
        . zip steps
        . zip (Nothing : steps')
        $ tail steps' ++ [Nothing]


hostVolumeSteps :: IO [Level]
hostVolumeSteps = different (error "unknown host")
    $ 風
        [ 00, 18, 20, 22, 25
        , 28, 32, 35, 40, 45
        , 50, 56, 63, 71, 79
        , 89, 100 ]
    . 空
        [ 00, 01, 03, 05, 07
        , 10, 13, 18, 23, 29
        , 36, 45, 56, 69, 85
        , 100 ]


indexCurrentOut :: IO String
indexCurrentOut = parsePacmd ["list-sinks"] id
    $ manyTill anyChar (string "* index: ")
   *> many1 digit


indexApp :: String -> IO String
indexApp app = parsePacmd ["list-sink-inputs"] id
    $ many (try tillIndex)
   *> many1 digit
   <* manyTill anyChar (try $ tillBinary app)

    where
    tillIndex  = manyTill anyChar . try $ string "index: "


volumeSteps :: IO Integer
volumeSteps = parsePacmd ["list-sinks"] read
    $ manyTill anyChar (char '*')
   *> manyTill anyChar (try $ string "volume steps:")
   *> skipMany1 space
   *> many1 digit


parsePacmd :: [String] -> (a -> b) -> Parser a -> IO b
parsePacmd args succeeded parser =
    (`fmap` readPacmd args) $
        either
            (error . show)
            succeeded
            . parse parser (head args)


readPacmd :: [String] -> IO String
readPacmd args = readProcess "pacmd" args ""


execPacmd :: [String] -> IO ()
execPacmd = void . readPacmd


type Level = Double

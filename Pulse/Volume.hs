module Pulse.Volume
    ( getVolume
    , getLinearVolume

    , isMuted
    , isMutedGlobal
    , toggleMute
    , toggleMuteGlobal
    , mute
    , muteGlobal
    , unmute
    , unmuteGlobal

    , decreaseVolume
    , increaseVolume
    , linearize

    , Level

    , getHostname
    ) where


import Control.Monad  (void, unless, when)
import Data.Maybe     (fromJust, fromMaybe)
import System.Process (readProcess)
import Text.ParserCombinators.Parsec


getVolume :: IO Level
getVolume = parsePacmd "list-sinks" read $ do
    manyTill anyChar $ char '*'
    manyTill anyChar . try $ string "volume: 0:"
    skipMany1 space
    manyTill digit $ string "%"


getLinearVolume :: IO Level
getLinearVolume = do
    steps <- hostVolumeSteps
    linearize steps `fmap` getVolume


linearize :: [Level] -> Level -> Level
linearize steps = fromJust . (`lookup` mapping)
    where
    mapping = zip steps
            $ map (* (100 / fromIntegral (length steps - 1)))
                  [0..]


isMutedIndex :: String -> IO Bool
isMutedIndex index = parsePacmd "list-sink-inputs" (== "yes") $ do
    manyTill anyChar . try $ string ("index: " ++ index)
    manyTill anyChar . try $ string "muted: "
    many1 letter


isMuted :: String -> IO Bool
isMuted app = parsePacmd "list-sink-inputs" (== "yes") $ do
    many1 $ try toMuted
    muted <- many1 letter
    manyTill anyChar . try $ toBinary
    return muted

    where
    toMuted  = manyTill anyChar . try $ string "muted: "
    toBinary = string $ "application.process.binary = \"" ++ app


isMutedGlobal :: IO Bool
isMutedGlobal = parsePacmd "list-sinks" (== "yes") $ do
    manyTill anyChar $ char '*'
    manyTill anyChar . try $ string "muted:"
    skipMany1 space
    manyTill letter newline


toggleMute' :: Bool -> String -> IO ()
toggleMute' global index = do
    muted <- if global
               then
                 isMutedGlobal
               else
                 isMutedIndex index

    let state = if muted
                  then "0"
                  else "1"

    void . pacmd $
        if global
          then
            ["set-sink-mute", index, state]
          else
            ["set-sink-input-mute", index, state]


toggleMute :: String -> IO ()
toggleMute app = toggleMute' False =<< getIndexOf app


toggleMuteGlobal :: IO ()
toggleMuteGlobal = toggleMute' True =<< getIndex


mute, unmute :: String -> IO ()
mute   app = flip unless (toggleMute app) =<< isMuted app
unmute app = flip when   (toggleMute app) =<< isMuted app


muteGlobal, unmuteGlobal :: IO ()
muteGlobal   = flip unless toggleMuteGlobal =<< isMutedGlobal
unmuteGlobal = flip when   toggleMuteGlobal =<< isMutedGlobal


decreaseVolume, increaseVolume :: IO ()
decreaseVolume = changeVolume fst
increaseVolume = changeVolume snd


changeVolume :: ((Maybe Level, Maybe Level) -> Maybe Level) -> IO ()
changeVolume selector = do
    volume <- getVolume
    vmap   <- volumeMap

    let newv = fromMaybe volume
             . selector
             . fromJust
             $ lookup volume vmap

    setVolume newv


setVolume :: Level -> IO ()
setVolume level = do
    current <- getIndex
    vsteps  <- fmap (subtract 1) getVolumeSteps

    let newv = round $ level / 100 * fromIntegral vsteps :: Integer

    void $ pacmd ["set-sink-volume", current, show newv]


volumeMap :: IO [(Level, (Maybe Level, Maybe Level))]
volumeMap = do
    steps <- hostVolumeSteps

    let steps' = map return steps

    return . zip steps
           . zip (Nothing : steps')
           $ tail steps' ++ [Nothing]


hostVolumeSteps :: IO [Level]
hostVolumeSteps = do
    host <- getHostname

    return $ case host of
        "kaze"   -> kazeSteps
        "heaven" -> heavenSteps
        _        -> error "illegal host"


kazeSteps, heavenSteps :: [Level]
kazeSteps = [ 0, 18, 20, 22, 25, 28, 32, 35, 40
            , 45, 50, 56, 63, 71, 79, 89, 100 ]

heavenSteps = [ 0, 1, 3, 5, 7, 10, 13, 18, 23
              , 29, 36, 45, 56, 69, 85, 100]


-- getIndex :: IO String
-- getIndex = parsePacmd "list-sinks" id $ do
--     manyTill anyChar $ char '*'
--     manyTill anyChar . try $ string "name:"
--     manyTill space $ char '<'
--     manyTill anyChar $ char '>'


getIndex :: IO String
getIndex = parsePacmd "list-sinks" id $ do
    manyTill anyChar $ string "* index: "
    many1 digit


getIndexOf :: String -> IO String
getIndexOf app = parsePacmd "list-sink-inputs" id $ do
    many1 $ try toIndex
    index <- many1 digit
    manyTill anyChar . try $ toBinary
    return index

    where
    toIndex = manyTill anyChar . try $ string "index: "
    toBinary  = do
        string "application.process.binary = \""
        string app
        char '"'


getVolumeSteps :: IO Integer
getVolumeSteps = parsePacmd "list-sinks" read $ do
    manyTill anyChar $ char '*'
    manyTill anyChar . try $ string "volume steps:"
    skipMany1 space
    manyTill digit newline


pacmd :: [String] -> IO String
pacmd args = readProcess "pacmd" args ""


parsePacmd :: String -> (a -> b) -> GenParser Char () a -> IO b
parsePacmd arg succeeded parser =
    fmap (either failed succeeded
               . parse parser "something")
         (pacmd [arg])

    where
    failed = const $ error "poop"


getHostname :: IO String
getHostname = fmap (takeWhile (/= '\n')) $ readProcess "hostname" [] ""


type Level = Double

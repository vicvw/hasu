module Volume
    ( getVolume
    , getLinearVolume
    , isMuted
    , toggleMute
    , decreaseVolume
    , increaseVolume
    , linearize
    , volumeMap
    , Level
    ) where


import Control.Monad    (void)
import Data.Either      (either)
import Data.Maybe       (fromJust, fromMaybe)
import System.Process   (readProcess)
import Text.ParserCombinators.Parsec


getVolume :: IO Level
getVolume = do
    sinks <- pacmd ["list-sinks"]

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
    sinks <- pacmd ["list-sinks"]

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


decreaseVolume :: IO ()
decreaseVolume = changeVolume fst


increaseVolume :: IO ()
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
    current <- getSink
    vsteps  <- fmap (subtract 1) getVolumeSteps

    let newv = round $ level / 100 * fromIntegral vsteps

    void $ pacmd ["set-sink-volume", current, show newv]


volumeMap :: IO [(Level, (Maybe Level, Maybe Level))]
volumeMap = do
    steps <- volumeSteps

    let steps' = map return steps

    return . zip steps
           . zip (Nothing : steps')
           $ tail steps' ++ [Nothing]

    where
    volumeSteps = do
        host <- hostname

        return $ case host of
            "paradise" -> paradise
            "heaven"   -> undefined

        where
        paradise = [ 0, 18, 20, 22, 25, 28, 32, 35, 40
                   , 45, 50, 56, 63, 71, 79, 89, 100 ]

        heaven   = []

        hostname = fmap (takeWhile (/= '\n')) $ readProcess "hostname" [] ""


getSink :: IO String
getSink = do
    sinks <- pacmd ["list-sinks"]

    either fail
           succeed
         $ parse parseCurrentSink "current sink" sinks

    where
    parseCurrentSink = do
        manyTill anyChar $ char '*'
        manyTill anyChar . try $ string "name:"
        manyTill space $ char '<'
        manyTill anyChar $ char '>'

    fail    = const $ error "poop."
    succeed = return


getVolumeSteps :: IO Integer
getVolumeSteps = do
    sinks <- pacmd ["list-sinks"]

    either fail
           succeed
         $ parse parseVolumeSteps "volume steps" sinks

    where
    parseVolumeSteps = do
        manyTill anyChar $ char '*'
        manyTill anyChar . try $ string "volume steps:"
        skipMany1 space
        manyTill digit newline

    fail    = const $ error "poop."
    succeed = return . read


amixer :: String -> IO String
amixer args = readProcess "amixer" (words args) ""


pacmd :: [String] -> IO String
pacmd args = readProcess "pacmd" args ""


linearize :: Level -> Level
linearize = fromJust . (`lookup` mapping)

    where
    steps = [ 0, 18, 20, 22, 25, 28, 32, 35, 40
            , 45, 50, 56, 63, 71, 79, 89, 100 ]

    mapping = steps
        `zip` map (* (100 / fromIntegral (length steps - 1)))
                  [0..]


getMatches :: (String, String, String, [String]) -> [String]
getMatches regex =
    let (_, _, _, matches) = regex :: (String, String, String, [String])
    in  matches


getFirstMatch :: (String, String, String, [String]) -> String
getFirstMatch = head . getMatches


type Level = Double

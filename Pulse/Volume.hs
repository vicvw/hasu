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
import Data.Maybe       (fromJust, fromMaybe)
import System.Process   (readProcess)
import Text.ParserCombinators.Parsec


getVolume :: IO Level
getVolume = parsePacmd read $ do
    manyTill anyChar $ char '*'
    manyTill anyChar . try $ string "volume: 0:"
    skipMany1 space
    manyTill digit $ string "%"


getLinearVolume :: IO Level
getLinearVolume = do
    steps <- hostVolumeSteps

    fmap (linearize steps) getVolume


linearize :: [Level] -> Level -> Level
linearize steps = fromJust . (`lookup` mapping)
    where
    mapping = zip steps
            $ map (* (100 / fromIntegral (length steps - 1)))
                  [0..]


isMuted :: IO Bool
isMuted = parsePacmd (== "yes") $ do
    manyTill anyChar $ char '*'
    manyTill anyChar . try $ string "muted:"
    skipMany1 space
    manyTill letter newline


toggleMute :: IO ()
toggleMute = do
    sink  <- getSink
    muted <- isMuted

    let state = if muted
                then "0"
                else "1"

    void $ pacmd ["set-sink-mute", sink, state]


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
    current <- getSink
    vsteps  <- fmap (subtract 1) getVolumeSteps

    let newv = round $ level / 100 * fromIntegral vsteps

    void $ pacmd ["set-sink-volume", current, show newv]


volumeMap :: IO [(Level, (Maybe Level, Maybe Level))]
volumeMap = do
    steps <- hostVolumeSteps

    let steps' = map return steps

    return . zip steps
           . zip (Nothing : steps')
           $ tail steps' ++ [Nothing]


hostVolumeSteps = do
    host <- getHostname

    return $ case host of
        "paradise" -> paradiseSteps
        "heaven"   -> undefined


paradiseSteps, heavenSteps :: [Level]
paradiseSteps = [ 0, 18, 20, 22, 25, 28, 32, 35, 40
                , 45, 50, 56, 63, 71, 79, 89, 100 ]

heavenSteps   = []


getSink :: IO String
getSink = parsePacmd id $ do
    manyTill anyChar $ char '*'
    manyTill anyChar . try $ string "name:"
    manyTill space $ char '<'
    manyTill anyChar $ char '>'


getVolumeSteps :: IO Integer
getVolumeSteps = parsePacmd read $ do
    manyTill anyChar $ char '*'
    manyTill anyChar . try $ string "volume steps:"
    skipMany1 space
    manyTill digit newline


pacmd :: [String] -> IO String
pacmd args = readProcess "pacmd" args ""


parsePacmd succeed parser =
    fmap (either fail succeed
               . parse parser "something")
         (pacmd ["list-sinks"])

    where
    fail = const $ error "poop"


getHostname :: IO String
getHostname = fmap (takeWhile (/= '\n')) $ readProcess "hostname" [] ""


type Level = Double

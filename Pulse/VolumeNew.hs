module VolumeNew
    ( toggleMuteApp
    ) where


-- import Omo            (different, onHost, 風, 空)


import Control.Applicative hiding ((<|>), many)
-- import Control.Arrow  ((&&&))
import Control.Monad  (void, unless, when)

import Data.Char      (toLower)
import Data.List      (isInfixOf)
-- import Data.Foldable  (forM_)
-- import Data.Maybe     (fromJust, fromMaybe)
-- import Data.Ord       (comparing)

import System.Process (readProcess)

import Text.ParserCombinators.Parsec


main :: IO ()
main = do
    p <- sinkInputs
    putStr . unlines $ map show p

    t <- appSinks "Chrom"
    print $ map inputIndex t

    -- toggleMuteApp "Chrom"


toggleMuteApp :: String -> IO ()
toggleMuteApp app = (=<< appSinks app) .
    mapM_ $ \(SinkInput i m _) ->
        toggleMute_ "set-sink-input-mute" i m


toggleMute :: IO ()
toggleMute = (=<< currentSink) $ \(Sink i m) ->
    toggleMute_ "set-sink-mute" i m


toggleMute_ :: String -> Integer -> Bool -> IO ()
toggleMute_ cmd index muted = execPacmd
    [ cmd
    , show index
    , if muted then "no" else "yes"
    ]


appSinks :: String -> IO [SinkInput]
appSinks app = (<$> sinkInputs) .
    filter $ isInfixOf app . inputName


sinkInputs :: IO [SinkInput]
sinkInputs = parsePacmd ["list-sink-inputs"] id $ do
    manyTill anyChar . try $ string "index: 0"

    many . try $ do
        index <- jumptTo "index: " $
            many1 digit

        muted <- jumptTo "muted: " $
            string "no" <|> string "yes"

        name  <- jumptTo "application.name = \"" $
            manyTill anyChar $ char '\"'

        return SinkInput
            { inputIndex = read index
            , inputMuted = toBool muted
            , inputName  = name
            }


currentSink :: IO Sink
currentSink = parsePacmd ["list-sinks"] id $ do
    index <- jumptTo "* index: " $
        many1 digit

    muted <- jumptTo "muted: " $
        string "no" <|> string "yes"

    return Sink
        { sinkIndex = read index
        , sinkMuted = toBool muted
        }


toBool :: String -> Bool
toBool "no"  = False
toBool "yes" = True


jumptTo :: String -> Parser a -> Parser a
jumptTo s = (*>) $ manyTill anyChar . try $ string s


parsePacmd :: [String] -> (a -> b) -> Parser a -> IO b
parsePacmd args = parsePacmdFail args $ error . show


parsePacmdFail :: [String] -> (ParseError -> b) -> (a -> b) -> Parser a -> IO b
parsePacmdFail args failed succeeded parser =
    (<$> readPacmd args) $ either
        failed
        succeeded
        . parse parser (head args)


execPacmd :: [String] -> IO ()
execPacmd = void . readPacmd


readPacmd :: [String] -> IO String
readPacmd args = readProcess "pacmd" args ""


data Sink = Sink
    { sinkIndex   :: Integer
    , sinkMuted   :: Bool
    } deriving Show


data SinkInput = SinkInput
    { inputIndex  :: Integer
    , inputMuted  :: Bool
    , inputName   :: String
    } deriving Show




-- volumeOut :: IO Level
-- volumeOut = do
--     current <- indexCurrentOut

--     parsePacmd ["list-sinks"] read
--         $ manyTill anyChar (string $ "* index: " ++ current)
--        *> manyTill anyChar (try $ string "volume:")
--        *> manyTill anyChar (char '/')
--        *> skipMany1 space
--        *> many1 digit


-- volumeOutLinear :: IO Level
-- volumeOutLinear = hostVolumeSteps >>=
--     (<$> volumeOut) . linearize

--     where
--     linearize steps = fromJust . (`lookup` mapping steps)

--     mapping steps = zip steps $
--         map (* (100 / fromIntegral (length steps - 1)))
--             [0..]


-- isMutedOut :: IO Bool
-- isMutedOut = do
--     current <- indexCurrentOut

--     parsePacmd ["list-sinks"] (== "yes")
--         $ manyTill anyChar (string $ "* index: " ++ current)
--        *> manyTill anyChar (try $ string "muted: ")
--        *> yesNo


-- isMutedApp :: String -> IO (Maybe Bool)
-- isMutedApp app = indexApp app >>= maybe
--     (return Nothing)
--     (\index ->
--     parsePacmd ["list-sink-inputs"] (Just . (== "yes"))
--         $ manyTill anyChar (try . string $ "index: " ++ index)
--        *> manyTill anyChar (try $ string "muted: ")
--        *> yesNo)


-- yesNo :: Parser String
-- yesNo = string "yes" <|> string "no"


-- tillApp :: String -> Parser String
-- tillApp app = try . manyTill anyChar $ stringI app

-- -- tillApp app = try $ string "application.process.binary = \""
-- --     *> manyTill anyChar (stringI app)

--     where
--     stringI = mapM (uncurry (<|>) . (char &&& char . toLower))


-- muteOut, unmuteOut :: IO ()
-- muteOut   = changeMuteOut unless
-- unmuteOut = changeMuteOut when


-- changeMuteOut :: (Bool -> IO () -> IO a) -> IO a
-- changeMuteOut cond = isMutedOut >>=
--     (`cond` toggleMuteOut)


-- muteApp, unmuteApp :: String -> IO ()
-- muteApp   = changeMuteApp unless
-- unmuteApp = changeMuteApp when


-- changeMuteApp :: (Bool -> IO () -> IO ()) -> String -> IO ()
-- changeMuteApp cond app = isMutedApp app >>= maybe
--     (return ())
--     (`cond` toggleMuteApp app)


-- decreaseVolumeOut, increaseVolumeOut :: IO ()
-- decreaseVolumeOut = changeVolumeOut fst
-- increaseVolumeOut = changeVolumeOut snd


-- changeVolumeOut :: ((Maybe Level, Maybe Level) -> Maybe Level) -> IO ()
-- changeVolumeOut selector = do
--     volume <- volumeOut
--     vmap   <- volumeMap

--     let newv = fromMaybe volume
--              . selector
--              . fromJust
--              . (`lookup` vmap)
--              $ closestOn fst volume vmap

--     setVolumeOut newv

--     where
--     closestOn f a = f . minimumBy (comparing $ abs . subtract a . f)


-- setVolumeOut :: Level -> IO ()
-- setVolumeOut level = do
--     current <- indexCurrentOut
--     vsteps  <- subtract 1 <$> volumeSteps

--     let newv = round $ level / 100 * fromIntegral vsteps

--     execPacmd ["set-sink-volume", current, show newv]


-- volumeMap :: IO [(Level, (Maybe Level, Maybe Level))]
-- volumeMap = do
--     steps <- hostVolumeSteps

--     let steps' = map return steps

--     return
--         . zip steps
--         . zip (Nothing : steps')
--         $ tail steps' ++ [Nothing]


-- hostVolumeSteps :: IO [Level]
-- hostVolumeSteps = different (error "unknown host")
--     $ 風
--         [ 00, 18, 20, 22, 25
--         , 28, 32, 35, 40, 45
--         , 50, 56, 63, 71, 79
--         , 89, 100 ]
--     . 空
--         [ 00, 01, 03, 05, 07
--         , 10, 13, 18, 23, 29
--         , 36, 45, 56, 69, 85
--         , 100 ]


-- indexCurrentOut :: IO String
-- indexCurrentOut = parsePacmd ["list-sinks"] id
--     $ manyTill anyChar (string "* index: ")
--    *> many1 digit


-- volumeSteps :: IO Integer
-- volumeSteps = parsePacmd ["list-sinks"] read
--     $ manyTill anyChar (char '*')
--    *> manyTill anyChar (try $ string "volume steps:")
--    *> skipMany1 space
--    *> many1 digit


-- type Level = Double

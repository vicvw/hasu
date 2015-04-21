module Volume
    ( toggleMute
    , toggleMuteApp
    , toggleMuteSink
    , isMuted
    , volume
    , decVolume
    , incVolume
    , setVolume
    , sinkInputs
    , SinkInput (..)
    ) where


import Omo            (different, 風, 空)


import Control.Applicative hiding ((<|>), many)
import Control.Monad  (void, unless, when)

import Data.List      (isInfixOf)
import Data.Foldable  (find, minimumBy)
import Data.Maybe     (fromJust)
import Data.Ord       (comparing)

import System.Process (readProcess)

import Text.ParserCombinators.Parsec


main :: IO ()
main = do
    sinks <- sinkInputs
    let s = unlines $ map show sinks

    out   <- readProcess "dmenu" ["-b", "-fn", "Noto Sans CJK JP:size=14"] s

    let i = read . takeWhile (/= ' ') $ out :: Integer
    toggleMuteSink . fromJust $ find ((== i) . inputIndex) sinks


isMuted :: IO Bool
isMuted = sinkMuted <$> currentSink


volume :: IO Double
volume = (<$> currentSink) $ \Sink { sinkVolume = v, sinkPoints = ps } ->
    linearize (fst <$> ps) v

    where
    linearize ps = fromJust . (`lookup` mapping ps)

    mapping ps = zip ps $ (<$> [0..])
        (* (100 / fromIntegral (length ps - 1)))


decVolume, incVolume :: IO ()
decVolume = changeVolume fst
incVolume = changeVolume snd


changeVolume :: ((Integer, Integer) -> Integer) -> IO ()
changeVolume sel = do
    Sink { sinkIndex  = i
         , sinkVolume = v
         , sinkPoints = ps } <- currentSink

    let v' = sel
           . fromJust
           . (`lookup` ps)
           $ closest v ps

    setVolume v'

    where
    closest v = fst . minimumBy (comparing $ abs . subtract v . fst)


setVolume :: Integer -> IO ()
setVolume v = do
    Sink { sinkIndex = i, sinkSteps = s } <- currentSink

    let v' = round $ fromIntegral v / 100 * fromIntegral (s - 1)

    execPacmd ["set-sink-volume", show i, show v']


toggleMuteApp :: String -> IO ()
toggleMuteApp app = (=<< appSinks app) .
    mapM_ $ \(SinkInput i m _) ->
        toggleMute_ "set-sink-input-mute" i m


toggleMuteSink :: SinkInput -> IO ()
toggleMuteSink (SinkInput i m _) =
    toggleMute_ "set-sink-input-mute" i m


toggleMute :: IO ()
toggleMute = (=<< currentSink) $ \(Sink i m _ _ _) ->
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
currentSink = do
    ps <- points

    parsePacmd ["list-sinks"] id $ do
        index <- jumptTo "* index: " $
            many1 digit

        vol   <- jumptTo "volume: " $ do
            manyTill anyChar $ char '/'
            spaces
            many1 digit

        steps <- jumptTo "volume steps: " $
            many1 digit

        muted <- jumptTo "muted: " $
            string "no" <|> string "yes"

        return Sink
            { sinkIndex  = read index
            , sinkMuted  = toBool muted
            , sinkVolume = read vol
            , sinkSteps  = read steps
            , sinkPoints = pointMap ps
            }

    where
    points = different [0, 10 .. 100]
        $ 風 [ 00, 18, 20, 22, 25, 28, 32, 35
             , 40, 45, 50, 56, 63, 71, 79, 89, 100 ]
        . 空 [ 00, 01, 03, 05, 07, 10, 13, 18
             , 23, 29, 36, 45, 56, 69, 85, 100 ]

    pointMap ps = zip ps . zip (minimum ps : ps) $ tail ps ++ [maximum ps]


toBool :: String -> Bool
toBool "no"  = False
toBool "yes" = True


jumptTo :: String -> Parser a -> Parser a
jumptTo s = (*>) $ manyTill anyChar . try $ string s


parsePacmd :: [String] -> (a -> b) -> Parser a -> IO b
parsePacmd args = parsePacmdFail args $ error . show


parsePacmdFail :: [String] -> (ParseError -> b) -> (a -> b) -> Parser a -> IO b
parsePacmdFail args bad good p = (<$> readPacmd args) $
    either bad good . parse p (head args)


execPacmd :: [String] -> IO ()
execPacmd = void . readPacmd


readPacmd :: [String] -> IO String
readPacmd args = readProcess "pacmd" args ""


instance Show SinkInput where
    show (SinkInput i m n) = concat
        [ show i
        , if m then " ・ " else " 　 "
        , n
        ]


data Sink = Sink
    { sinkIndex   :: Integer
    , sinkMuted   :: Bool
    , sinkVolume  :: Integer
    , sinkSteps   :: Integer
    , sinkPoints  :: [(Integer, (Integer, Integer))]
    } deriving Show


data SinkInput = SinkInput
    { inputIndex  :: Integer
    , inputMuted  :: Bool
    , inputName   :: String
    }




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

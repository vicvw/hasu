module Cmus
    ( isRunning
    , query
    , play
    , toggle
    , stop
    , previous
    , next
    , fromDuration
    , CmusMetadata (..)
    , CmusStatus (..)
    ) where


import Text.ParserCombinators.Parsec

import Control.Applicative  ((<$>), (<*), (*>))
import Control.Monad        (void, when)

import System.Exit          (ExitCode (..))
import System.Process       (readProcess, readProcessWithExitCode)


play, toggle, stop, previous, next :: IO ()
play     = whenRunning Play
toggle   = whenRunning Toggle
stop     = whenRunning Stop
previous = whenRunning Previous
next     = whenRunning Next


whenRunning :: CmusCommand -> IO ()
whenRunning cmd = do
    running <- isRunning
    when running . void $ cmus cmd


query :: IO (Maybe CmusMetadata)
query = (<$> cmus Query) $
    fmap (either (error . show) id
                 . parse parseQuery "cmus-remote -Q")

    where
    parseQuery = do
        status              <- parseGenericMany       "status"   letter
        file                <- opt $ parseGenericMany "file"     anyChar
        duration            <- opt $ parseGeneric     "duration" $ (string "-1" <|> many1 digit) <* newline
        position            <- opt $ parseGenericMany "position" digit

        artist              <- parseTag "artist"                 anyChar
        album               <- parseTag "album"                  anyChar
        title               <- parseTag "title"                  anyChar
        date                <- parseTag "date"                   anyChar
        genre               <- parseTag "genre"                  anyChar
        discNumber          <- parseTag "discnumber"             digit
        trackNumber         <- parseTag "tracknumber"            digit
        albumArtist         <- parseTag "albumartist"            anyChar
        composer            <- parseTag "composer"               anyChar
        conductor           <- parseTag "conductor"              anyChar
        comment             <- parseTag "comment"                anyChar
        replayGainTrackGain <- parseTag "replaygain_track_gain"  anyChar

        stream              <- opt $ parseGenericMany "stream"   anyChar

        aaaMode             <- parseSet "aaa_mode"               letter
        continue            <- parseSet "continue"               letter
        playLibrary         <- parseSet "play_library"           letter
        playSorted          <- parseSet "play_sorted"            letter
        replayGain          <- parseSet "replaygain"             letter
        replayGainLimit     <- parseSet "replaygain_limit"       letter
        replayGainPreAmp    <- parseSet "replaygain_preamp"      $ digit <|> char '.'
        repeat_             <- parseSet "repeat"                 letter
        repeatCurrent       <- parseSet "repeat_current"         letter
        shuffle             <- parseSet "shuffle"                letter
        softVolume          <- parseSet "softvol"                letter
        volumeLeft          <- parseSet "vol_left"               digit
        volumeRight         <- parseSet "vol_right"              digit

        return CmusMetadata
            { _status               = toStatus status
            , _file                 = toMaybe file
            , _duration             = toDuration `fmap` toMaybe duration
            , _position             = read `fmap` toMaybe position

            , _artist               = toMaybe artist
            , _album                = toMaybe album
            , _title                = toMaybe title
            , _date                 = toMaybe date
            , _genre                = toMaybe genre
            , _discNumber           = read `fmap` toMaybe discNumber
            , _trackNumber          = read `fmap` toMaybe trackNumber
            , _albumArtist          = toMaybe albumArtist
            , _composer             = toMaybe composer
            , _conductor            = toMaybe conductor
            , _comment              = toMaybe comment
            , _replayGainTrackGain  = toMaybe replayGainTrackGain

            , _stream               = toMaybe stream

            , _aaaMode              = toAAAMode aaaMode
            , _continue             = toBool continue
            , _playLibrary          = toBool playLibrary
            , _playSorted           = toBool playSorted
            , _replayGain           = toBool replayGain
            , _replayGainLimit      = toBool replayGainLimit
            , _replayGainPreAmp     = read replayGainPreAmp
            , _repeat               = toBool repeat_
            , _repeatCurrent        = toBool repeatCurrent
            , _shuffle              = toBool shuffle
            , _softVolume           = toBool softVolume
            , _volumeLeft           = read volumeLeft
            , _volumeRight          = read volumeRight
            }

    opt = option "" . try

    parseGeneric str parser
        = string str
       *> space
       *> parser

    parseGenericMany str parser
        = parseGeneric str
        $ manyTill parser newline

    parseTag str parser
        = opt
        $ string "tag"
       *> space
       *> string str
       *> space
       *> manyTill parser newline

    parseSet str parser
        = string "set"
       *> space
       *> string str
       *> space
       *> manyTill parser newline

    toStatus str = case str of
        "playing" -> Playing
        "paused"  -> Paused
        "stopped" -> Stopped
        _         -> error "weird status"

    toDuration str = case str of
        "-1" -> Infinity
        _    -> Bounded $ read str

    toMaybe str = case str of
        "" -> Nothing
        _  -> Just str

    toAAAMode str = case str of
        "all"    -> All
        "artist" -> Artist
        "album"  -> Album

    toBool str = case str of
        "true"  -> True
        "false" -> False

        "enabled"  -> True
        "disabled" -> False


isRunning :: IO Bool
isRunning = do
    (code, _, _) <- readProcessWithExitCode "cmus-remote" ["-Q"] ""

    return $ case code of
        ExitSuccess   -> True
        ExitFailure _ -> False


cmus :: CmusCommand -> IO (Maybe String)
cmus cmd = do
    running <- isRunning

    if running
    then Just <$> readProcess "cmus-remote" [show cmd] ""
    else return Nothing


instance Show CmusCommand where
    show cmd = case cmd of
        Query    -> "-Q"
        Play     -> "--play"
        Toggle   -> "--pause"
        Stop     -> "--stop"
        Previous -> "--prev"
        Next     -> "--next"


data CmusCommand
    = Query
    | Play
    | Toggle
    | Stop
    | Previous
    | Next


data CmusMetadata = CmusMetadata
    { _status               :: CmusStatus
    , _file                 :: Maybe String
    , _duration             :: Maybe Duration
    , _position             :: Maybe Integer

    , _artist               :: Maybe (Tag String)
    , _album                :: Maybe (Tag String)
    , _title                :: Maybe (Tag String)
    , _date                 :: Maybe (Tag String)
    , _genre                :: Maybe (Tag String)
    , _discNumber           :: Maybe (Tag Integer)
    , _trackNumber          :: Maybe (Tag Integer)
    , _albumArtist          :: Maybe (Tag String)
    , _composer             :: Maybe (Tag String)
    , _conductor            :: Maybe (Tag String)
    , _comment              :: Maybe (Tag String)
    , _replayGainTrackGain  :: Maybe (Tag String)

    , _stream               :: Maybe String

    , _aaaMode              :: Set AAAMode
    , _continue             :: Set Bool
    , _playLibrary          :: Set Bool
    , _playSorted           :: Set Bool
    , _replayGain           :: Set Bool
    , _replayGainLimit      :: Set Bool
    , _replayGainPreAmp     :: Set Double
    , _repeat               :: Set Bool
    , _repeatCurrent        :: Set Bool
    , _shuffle              :: Set Bool
    , _softVolume           :: Set Bool
    , _volumeLeft           :: Set Integer
    , _volumeRight          :: Set Integer
    } deriving (Show)


type Tag a = a
type Set a = a


data CmusStatus
    = Playing
    | Paused
    | Stopped
    deriving (Show, Eq)


data Duration
    = Bounded Integer
    | Infinity
    deriving (Show)


fromDuration :: Duration -> Integer
fromDuration dur = case dur of
    Bounded t -> t
    Infinity  -> 10^6


data AAAMode
    = All
    | Artist
    | Album
    deriving (Show)

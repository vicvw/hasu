module Cmus.Query
    ( query
    , fromDuration
    , Metadata (..)
    , Status (..)
    ) where


import Cmus.General


import Text.ParserCombinators.Parsec

import Control.Applicative  ((<$>), (*>))


query :: IO (Maybe Metadata)
query = (<$> cmus Query) $ \mq -> (<$> mq) $ \q -> Metadata
    { _status           = toStatus   . parseQuery      q "status"                 $ many1 letter
    , _file             =              parseQueryMaybe q "file"                     manyNotNewline
    , _duration         = fmap toDur . parseQueryMaybe q "duration"               $ string "-1" <|> many1 digit
    , _position         = fmap read  . parseQueryMaybe q "position"               $ many1 digit

    , _artist           =              parseQueryMaybe q "tag artist"               manyNotNewline
    , _album            =              parseQueryMaybe q "tag album"                manyNotNewline
    , _title            =              parseQueryMaybe q "tag title"                manyNotNewline
    , _discNumber       = fmap read  . parseQueryMaybe q "tag discNumber"         $ many digit
    , _trackNumber      = fmap read  . parseQueryMaybe q "tag trackNumber"        $ many digit
    , _albumArtist      =              parseQueryMaybe q "tag albumArtist"          manyNotNewline

    , _stream           =              parseQueryMaybe q "stream"                   manyNotNewline

    , _aaaMode          = toAAAMode  . parseQuery      q "set aaa_mode"           $ many letter
    , _continue         = toBool     . parseQuery      q "set continue"           $ many letter
    , _playLibrary      = toBool     . parseQuery      q "set play_library"       $ many letter
    , _playSorted       = toBool     . parseQuery      q "set play_sorted"        $ many letter
    , _replayGain       = toBool     . parseQuery      q "set replaygain"         $ many letter
    , _replayGainLimit  = toBool     . parseQuery      q "set replaygain_limit"   $ many letter
    , _replayGainPreAmp = read       . parseQuery      q "set replaygain_preamp"  . many $ digit <|> char '.'
    , _repeat           = toBool     . parseQuery      q "set repeat"             $ many letter
    , _repeatCurrent    = toBool     . parseQuery      q "set repeat_current"     $ many letter
    , _shuffle          = toBool     . parseQuery      q "set shuffle"            $ many letter
    , _softVolume       = toBool     . parseQuery      q "set softvol"            $ many letter
    , _volumeLeft       = read       . parseQuery      q "set vol_left"           $ many digit
    , _volumeRight      = read       . parseQuery      q "set vol_right"          $ many digit
    }

    where
    parseQuery      = parseQuery' $ either (error . show) id
    parseQueryMaybe = parseQuery' $ either (const Nothing) Just

    parseQuery' f q str parser = f $ parse parser' str q
        where
        parser' = manyTill anyChar (try $ string str)
               *> space
               *> parser

    manyNotNewline = manyTill anyChar newline

    toStatus str = case str of
        "playing" -> Playing
        "paused"  -> Paused
        "stopped" -> Stopped
        s         -> error $ "weird status: " ++ s

    toDur str = case str of
        "-1" -> Infinity
        _    -> Bounded $ read str

    toAAAMode str = case str of
        "all"    -> All
        "artist" -> Artist
        "album"  -> Album

    toBool str = case str of
        "true"  -> True
        "false" -> False

        "enabled"  -> True
        "disabled" -> False


data Metadata = Metadata
    { _status               :: Status
    , _file                 :: Maybe String
    , _duration             :: Maybe Duration
    , _position             :: Maybe Integer

    , _artist               :: Maybe (Tag String)
    , _albumArtist          :: Maybe (Tag String)
    , _album                :: Maybe (Tag String)
    , _title                :: Maybe (Tag String)
    , _discNumber           :: Maybe (Tag Integer)
    , _trackNumber          :: Maybe (Tag Integer)

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


data Status
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

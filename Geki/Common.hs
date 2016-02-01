module Common where


import Control.Monad      ((<=<))

import Network.HTTP       (getRequest, getResponseBody, simpleHTTP)

import System.Timeout     (timeout)

import Text.Parsec        (parse)
import Text.Parsec.String (Parser)


parseEpisodes :: String -> Parser [Episode] -> String -> [Episode]
parseEpisodes s p = either (error . show) id . parse p s


getUrl :: String -> IO (Maybe String)
getUrl = timeout 5000000 . getResponseBody <=< simpleHTTP . getRequest


data Episode = Episode
    { _site :: Site
    , _name :: String
    , _ep   :: Int
    , _sub  :: Maybe Int
    } deriving (Show, Read, Eq)


data Site
    = DramaBay
    | DramaFire
    | DramaNet
    | DramaNice
    | IKShow
    | MyAsianFever
    | MyAsianTV
    deriving (Show, Read, Eq)

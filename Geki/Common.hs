module Common where --(main) where


import Control.Monad      ((<=<))

import Network.HTTP       (getRequest, getResponseBody, simpleHTTP)

import System.Timeout     (timeout)

import Text.Parsec        (parse)
import Text.Parsec.String (Parser)


parseEpisodes :: String -> Parser [Episode] -> String -> [Episode]
parseEpisodes s p = either (error . show) id . parse p s


getURL :: String -> IO (Maybe String)
getURL = timeout 5000000
    . getResponseBody <=< simpleHTTP . getRequest


data Episode = Episode
    { site  :: Site
    , name  :: String
    , ep    :: Int
    , sub   :: Maybe Int
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

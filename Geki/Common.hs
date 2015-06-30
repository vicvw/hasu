module Common where --(main) where


import Control.Monad      ((<=<))

import Network.HTTP       (getRequest, getResponseBody, simpleHTTP)

import Text.HTML.TagSoup  (parseTags, Tag)
import Text.ParserCombinators.Parsec  (parse, Parser)


parseEpisodes :: String -> Parser [Episode] -> String -> [Episode]
parseEpisodes s p = either (error . show) id . parse p s


getTags :: String -> IO [Tag String]
getTags url = parseTags <$> getURL url


getURL :: String -> IO String
getURL = getResponseBody <=< simpleHTTP . getRequest


data Episode = Episode
    { site  :: Site
    , name  :: String
    , ep    :: Int
    , sub   :: Maybe Int
    } deriving (Show, Read, Eq)


data Site
    = DramaBay
    | DramaCool
    | DramaFire
    | DramaFever
    | MyAsianTV
    deriving (Show, Read, Eq)

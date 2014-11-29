module Kiss (main) where


import Control.Applicative      ((<$>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad            ((<=<))

import Data.List                (find, isInfixOf)
import Data.Maybe               (isJust)

import Network.HTTP             (getRequest, getResponseBody, simpleHTTP)

import Text.HTML.TagSoup        ((~==), fromAttrib, fromTagText, parseTags, sections, Tag (TagOpen))

import System.Environment       (getArgs)


main :: IO ()
main = do
    title <- head <$> getArgs
    links <- episodeLinks title
    tagss <- mapConcurrently getTags links

    putStrLn . concat . concat . reverse . (<$> tagss) . (. flip id) $
        (<$> [ fromTagText . head . drop 2 . concat . sections (~== "Filename:")
             , firstLink . concat . sections (~== "<div id='divDownload'>")
             , const "\n"
             ])

    where
    episodeLinks title = (<$> getTags ("/Anime/" ++ title)) $
        episodes . concat . sections (~== "<table class='listing'>")

    firstLink = head . hrefs

    episodes = filter ("?id" `isInfixOf`) . hrefs
    hrefs ss = fromAttrib "href" <$>
               [ tag
               | tag@(TagOpen "a" attrs) <- ss
               , isJust $ find ((== "href") . fst) attrs
               ]


getTags :: String -> IO [Tag String]
getTags = (parseTags <$>) . getURL . kiss


getURL :: String -> IO String
getURL = getResponseBody <=< simpleHTTP . getRequest


kiss :: String -> String
kiss = ("http://kissanime.com" ++)

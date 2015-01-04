module Main (main) where


import Control.Applicative      ((<$>))
import Control.Arrow            ((&&&))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad            ((<=<))

import Data.List                (find, isInfixOf)
import Data.Maybe               (isJust)

import Network.HTTP             (getRequest, getResponseBody, simpleHTTP)

import Text.HTML.TagSoup        ((~==), fromAttrib, fromTagText, parseTags, sections, Tag (TagOpen))
import Text.Printf              (printf)

import System.Environment       (getArgs)


main :: IO ()
main = do
    title <- head <$> getArgs
    -- links <- episodeLinks title
    let links = episodeFiles $ read title
    tagss <- mapConcurrently getTagsF links

    mapM_ (putStrLn . wget) . reverse . (<$> tagss) $
            filename  . sections' "Filename:"
        &&& firstLink . sections' "<div id='divDownload'>"

    where
    episodeLinks title = (<$> getTags ("/Anime/" ++ title)) $
        episodes . concat . sections (~== "<table class='listing'>")

    episodeFiles n = printf "Episode-0%02d.html" <$> [1..n :: Integer]

    filename  = fromTagText . head . drop 2
    firstLink = head . hrefs

    episodes = filter ("?id" `isInfixOf`) . hrefs
    hrefs ss = fromAttrib "href" <$>
               [ tag
               | tag@(TagOpen "a" attrs) <- ss
               , isJust $ find ((== "href") . fst) attrs
               ]

    sections' s = concat . sections (~== s)


wget :: (String, String) -> String
wget (title, link) = concat
    [ "wget -q -O '"
    , filter (/= '\n') title
    , ".mp4' '"
    , link
    , "'&"
    ]


getTags :: String -> IO [Tag String]
getTags = (parseTags <$>) . getURL . kiss


getTagsF :: String -> IO [Tag String]
getTagsF = (parseTags <$>) . readFile


getURL :: String -> IO String
getURL = getResponseBody <=< simpleHTTP . getRequest


kiss :: String -> String
kiss = ("http://kissanime.com" ++)

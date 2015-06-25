module Main (main) where


import Control.Monad      ((<=<), forM_, when)

import Data.List          ((\\), union)

import Network.HTTP       (getRequest, getResponseBody, simpleHTTP)

import System.Environment (getArgs)
import System.IO          (appendFile, writeFile)
import qualified System.IO.Strict as SIO  (readFile)

import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions, Tag)
import Text.ParserCombinators.Parsec


main :: IO ()
main = do
    有:_ <- getArgs
    tags <- getTags
    白   <- r白

    let 劇 = filter ((`elem` 白) . name)
           $ parseEpisode <$> recent tags
        -- 劇' = Episode "Mask" 11 Nothing : Episode "Mask" 10 Nothing : tail (tail 劇)

    劇古 <- r劇
    未   <- r未

    let 劇新 = 劇 \\ union 劇古 未

    w劇 劇
    a未 劇新

    let n = length 未
    when (n > 0) . putStr $ show n ++ 有

    -- mapM_ print $ 劇新
    -- putStrLn ""
    -- mapM_ print 未

    where
    r白 = lines <$> SIO.readFile (ぶ "白")

    w劇 = writeFile  (ぶ "劇") . show'
    a未 = appendFile (ぶ "未") . show'

    r劇 = read' <$> SIO.readFile (ぶ "劇")
    r未 = read' <$> SIO.readFile (ぶ "未")

    ぶ  = ("/home/v/ぶ/Geki/" ++)


    show' :: [Episode] -> String
    show' eps = unlines $ show <$> eps

    read' :: String -> [Episode]
    read' = map read . lines


    parseEpisode = either (error "") id . parse episode "episode"

    episode = do
        name <- manyTill anyChar . try $ string " Ep "
        ep   <- many1 digit
        sub  <- optionMaybe . try $ string " (" *> string "Sub: " *> many1 digit
        return $ Episode name (read ep) (read <$> sub)

    recent
        = filter (not . (`elem` "\r ") . head)
        . map fromTagText
        . filter isTagText
        . takeWhile (~/= "</div>")
        . concat
        . partitions (~== "<ul class='list lastest'>")


getTags :: IO [Tag String]
getTags = parseTags <$> getURL mat


getURL :: String -> IO String
getURL = getResponseBody <=< simpleHTTP . getRequest


mat :: String
mat = "http://myasiantv.com"


data Episode = Episode
    { name  :: String
    , ep    :: Int
    , sub   :: Maybe Int
    } deriving (Show, Read, Eq)

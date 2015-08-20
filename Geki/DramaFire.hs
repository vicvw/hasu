module DramaFire where


import Common


import Nara


import Data.List          (isInfixOf)
import Data.Maybe         (isNothing)

import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions, Tag)
import Text.Parsec
import Text.Parsec.String (Parser)
import Debug.Trace


url :: String
url = "http://dramafire.com"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string " episode"
    optional $ char 's'
    space
    ep   <- many1 digit
    more <- optionMaybe $ char '-'

    fi (isNothing more)
        (return [Episode DramaFire name (read ep) Nothing])
        $ do
            ep' <- many1 digit <?> name
            return $ (<$> [read ep .. read ep']) $ \n ->
                Episode DramaFire name n Nothing


links :: [Tag String] -> [String]
links
    = traceShowId
    . filter ("episode" `isInfixOf`)
    . map fromTagText
    . filter isTagText
    . takeWhile (~/= "<div class='recent-from recent-last et-recent-top'>")
    . concat
    . partitions (~== "<div class='recent-from et-recent-top'>")

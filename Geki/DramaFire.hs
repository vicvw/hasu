module DramaFire where


import Common


import Data.List          (isInfixOf)

import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions, Tag)
import Text.Parsec
import Text.Parsec.String (Parser)


url :: String
url = "http://dramafire.com"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string " episode "
    ep   <- many1 digit
    return $ [Episode DramaFire name (read ep) Nothing]


links :: [Tag String] -> [String]
links
    = filter ("episode" `isInfixOf`)
    . map fromTagText
    . filter isTagText
    . takeWhile (~/= "<div class='recent-from recent-last et-recent-top'>")
    . concat
    . partitions (~== "<div class='recent-from et-recent-top'>")

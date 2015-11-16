module DramaNet where


import Common


import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions, Tag)
import Text.Parsec
import Text.Parsec.String (Parser)


url :: String
url = "http://www.drama.net/from-five-to-nine/"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string "\160Episode "
    ep   <- many1 digit
    return [Episode DramaNet name (read ep) Nothing]


links :: [Tag String] -> [String]
links
    = filter (not . (`elem` "\n ") . head)
    . map fromTagText
    . filter isTagText
    . takeWhile (~/= "</ul>")
    . concat
    . partitions (~== "<ul class='anime-list'>")

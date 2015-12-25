module IKShow where


import Common


import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions, Tag)
import Text.Parsec
import Text.Parsec.String (Parser)


url :: String
url = "http://ikshow.net/shows/k-pop-star-s5/"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string " Episode "
    ep   <- many1 digit
    return [Episode IKShow name (read ep) Nothing]


links :: [Tag String] -> [String]
links
    = filter ((/= 'o') . last)
    . filter (not . (`elem` "S\r\n ") . head)
    . map fromTagText
    . filter isTagText
    . takeWhile (~/= "</tbody>")
    . concat
    . partitions (~== "<div id='list-episodes'>")

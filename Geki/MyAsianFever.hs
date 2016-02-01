module MyAsianFever where


import Control.Monad      (void)
import Data.List          (intercalate, isInfixOf)

import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions)
import Text.Parsec
import Text.Parsec.String (Parser)


import Common


url :: String
url = "http://myasianfever.tv"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar $ char '|'
    void . try $ string "Episode " <|> string "S01E"
    ep   <- many1 digit
    return [Episode MyAsianFever name (read ep) Nothing]


links :: String -> [String]
links
    = filter (not . isInfixOf "Complete")
    . map (intercalate "|")
    . chunk 2
    . filter (not . (`elem` "\n\r ") . head)
    . map fromTagText
    . filter isTagText
    . takeWhile (~/= "</ul>")
    . concat
    . partitions (~== "<div id='series'")
    . parseTags

    where
    chunk n
        = foldr ((:) . take n) []
        . takeWhile (not . null)
        . iterate (drop n)

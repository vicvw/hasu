module MyAsianTV where


import Common


import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions, Tag)
import Text.Parsec
import Text.Parsec.String (Parser)


url :: String
url = "http://myasiantv.com"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string " Ep "
    ep   <- many1 digit
    sub  <- optionMaybe . try $ string " (" *> string "Sub: " *> many1 digit
    return $ [Episode MyAsianTV name (read ep) (read <$> sub)]


links :: [Tag String] -> [String]
links
    = filter (not . (`elem` "\r ") . head)
    . map fromTagText
    . filter isTagText
    . takeWhile (~/= "</div>")
    . concat
    . partitions (~== "<ul class='list lastest'>")

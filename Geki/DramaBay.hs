module DramaBay where


import Common

import Nara


import Data.List          (isPrefixOf)

import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions, Tag)
import Text.Parsec
import Text.Parsec.String (Parser)


url :: String
url = "http://www.dramabay.com/new-episodes"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ (lookAhead $ string "Special") <|> (string " E" <* notFollowedBy letter)
    sp   <- lookAhead $ many1 anyChar

    fi (sp == "Special")
        (return [Episode DramaBay name 0 Nothing])
        $ do
            a    <- many1 digit
            look <- lookAhead $ many anyChar

            b    <- fi (null look || " (" `isPrefixOf` look)
                        (return a)
                        $ do
                            manyTill anyChar $ char 'E'
                            many1 digit

            return $ (<$> [read a .. read b]) $ \n ->
                Episode DramaBay name n Nothing


links :: [Tag String] -> [String]
links
    = tail
    . filter (not . (`elem` "\n\r ") . head)
    . map fromTagText
    . filter isTagText
    . takeWhile (~/= "</ul>")
    . concat
    . partitions (~== "<div id='col2'")

module MyAsianFever (spec) where


import Control.Monad      (void)
import Data.List          (intercalate, isInfixOf)

import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions)
import Text.Parsec


import Common


spec :: Spec
spec = Spec
    { url = "http://myasianfever.tv"

    , parser = do
        name <- manyTill anyChar $ char '|'
        optional $ string "Season 2 "
        void . try $ string "Episode " <|> string "S01E"
        ep   <- many1 digit
        return [Episode MyAsianFever name (read ep) Nothing]

    , links
        = filter (not . isInfixOf "Complete")
        . map (intercalate "|" . take 2)
        . chunk 5
        . filter (not . (`elem` "\n\r ") . head)
        . map fromTagText
        . filter isTagText
        . takeWhile (~/= "</ul>")
        . concat
        . partitions (~== "<div id='series'")
        . parseTags
    }

    where
    chunk n
        = foldr ((:) . take n) []
        . takeWhile (not . null)
        . iterate (drop n)

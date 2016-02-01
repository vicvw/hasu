module DramaNice where


import Common


import Text.Parsec
import Text.Parsec.String (Parser)
import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hread, removeAllWhiteSpace, runLA, (>>>), (/>))


url :: String
url = "http://dramanice.to"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string " Episode "
    ep   <- many1 digit
    return [Episode DramaNice name (read ep) Nothing]


links :: String -> [String]
links html
    = ($ html)
    . runLA
    $ hread
    >>> css "#recentadd .sub .info-name a span"
    >>> removeAllWhiteSpace
     /> getText

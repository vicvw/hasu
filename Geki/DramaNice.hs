module DramaNice where


import Data.List

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hread, removeAllWhiteSpace, runLA, (>>>), (/>), (<+>))


import Common


url :: String
url = "http://dramanice.to"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string " Episode "
    ep   <- many1 digit
    return [Episode DramaNice name (read ep) Nothing]


links :: String -> [String]
links html
    = nub
    . ($ html)
    . runLA
    $ hread
    >>> css "#recentadd" <+> css "#recentpopular"
    >>> css ".sub .info-name a span"
    >>> removeAllWhiteSpace
     /> getText
